module Test.MultiunitAuction.Auction where
import Lorentz.Value
import qualified Lorentz as L

import Cleveland.Util (sec)
import qualified Michelson.Typed as T
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Traversable.WithIndex (TraversableWithIndex, ifor)
import qualified Hedgehog.Gen as Gen 
import qualified Hedgehog.Range as Range
import Hedgehog (Gen, Property, forAll, label, property)
import Morley.Nettest
import Test.MultiunitAuction.Util
import qualified Indigo.Contracts.FA2Sample as FA2

import qualified Lorentz.Contracts.MultiunitAuction.Auction as Auction
import Tezos.Core (timestampPlusSeconds)
import Fmt ((+|), (|+))
import Test.Util

hprop_First_bid_is_valid_IFF_it_meets_price_floor :: Property
hprop_First_bid_is_valid_IFF_it_meets_price_floor =
  property $ do

    testData@TestData{testPriceFloor} <- forAll genTestData
    firstBid <- forAll $ genMutez' (Range.linear 0 (testPriceFloor * 2))

    clevelandProp $ do
      setup@Setup{seller, startTime, contract} <- testSetup testData
      bidder <- newAddress "bidder"
      withSender seller $ addSampleBondingCurve contract
      withSender seller $ configureAuction testData setup
      waitForAuctionToStart testData

      withSender bidder $
        if firstBid >= testPriceFloor
          then placeBid contract 1 firstBid
          else placeBid contract 1 firstBid 
               `expectFailure` 
                  failedWith contract
                    ([mt|INVALID_BID_AMOUNT|], (bidder, firstBid, startTime, startTime))

----------------------------------------------------------------------------
-- Generators
----------------------------------------------------------------------------

data TestData = TestData
  { testRoundTime :: Natural
  , testExtendTime :: Natural
  , testTimeToStart :: Integer
  , testAuctionDuration :: Natural
  , testMaxAuctionTime :: Natural

  , testPriceFloor :: Mutez
  , testFirstBid :: Mutez
  }
  deriving stock (Show)

genTestData :: Gen TestData
genTestData = do
  testRoundTime <- Gen.integral (Range.linear 1 100)
  testExtendTime <- Gen.integral (Range.linear 1 100)

  testMaxConfigToStartTime <- Gen.integral (Range.linear 0 100)
  testTimeToStart <- Gen.integral (Range.linear 0 (toInteger testMaxConfigToStartTime))

  testPriceFloor <- genMutez' (Range.linear 1 1000)
  testFirstBid <- genMutez' (Range.linear testPriceFloor (testPriceFloor + 1000))

  testAuctionDuration <- Gen.integral (Range.linear 1 100)
  testMaxAuctionTime <- Gen.integral (Range.linear testAuctionDuration (testAuctionDuration * 2))

  pure $ TestData {..}

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

sampleBondingCurve :: '[Natural] L.:-> '[Mutez]
sampleBondingCurve = do
  L.push (toMutez 333333 :: Mutez) L.# L.mul

sampleIntegralCurve :: '[Natural] L.:-> '[Mutez]
sampleIntegralCurve = do
  L.dup L.# L.mul L.# L.push (toMutez 166666 :: Mutez) L.# L.mul 

data Setup = Setup
  { contract :: TAddress $ Auction.AuctionEntrypoints
  , fa2Contract :: TAddress FA2.FA2SampleParameter
  , feeCollector :: Address
  , seller :: Address
  , startTime :: Timestamp
  , endTime :: Timestamp
  , profitAddress :: Address
  , reserveAddress :: Address
  }

testSetup :: MonadNettest caps base m => TestData -> m Setup
testSetup testData = do
  feeCollector <- newAddress "fee-collector"
  reserveAddress <- newAddress "reserve-address"
  profitAddress <- newAddress "profit-address"
  seller <- newAddress "seller"

  contract <- originateAuction seller

  -- The FA2 contracts with the seller's assets
  fa2Contract <- 
    originateSimple "asset-fa2"
      FA2.Storage
      { sLedger = BigMap $ Map.empty --Auction contract mints tokens
      , sOperators = BigMap $ Map.fromList [((seller, toAddress contract), ())]
      , sTokenMetadata = mempty
      }
      (FA2.fa2Contract def { FA2.cAllowedTokenIds = []})

  now <- getNow
  let startTime = now `timestampPlusSeconds` testTimeToStart testData
  let endTime = startTime `timestampPlusSeconds` fromIntegral (testAuctionDuration testData)

  pure Setup {..}

waitForAuctionToStart :: (HasCallStack, MonadNettest caps base m) => TestData -> m ()
waitForAuctionToStart TestData{testTimeToStart} =
  advanceTime (sec $ fromIntegral testTimeToStart)

mkBidders :: (MonadNettest caps base m, TraversableWithIndex Int f) => f Mutez -> m (f Address)
mkBidders bids =
  ifor bids \i _ -> newAddress ("bidder-" <> show i)

getAuction :: MonadNettest caps base m => Auction.AuctionId -> Auction.AuctionStorage -> m Auction.Auction 
getAuction auctionId st = do
  let auctionOpt =
        st
          & Auction.auctions
          & unBigMap
          & Map.lookup auctionId
  case auctionOpt of
    Just auction -> pure auction
    Nothing -> failure $ "Expected the storage to contain an auction with ID '" +| auctionId |+ "', but it didn't."

----------------------------------------------------------------------------
-- Call entrypoints
----------------------------------------------------------------------------

configureAuction :: (HasCallStack, MonadNettest caps base m) => TestData -> Setup -> m ()
configureAuction testData Setup{fa2Contract, contract, startTime, endTime, reserveAddress, profitAddress}  = do

  call contract (Call @"Configure") Auction.ConfigureParam
    { priceFloor = testPriceFloor testData
    , roundTime = testRoundTime testData
    , extendTime = testExtendTime testData
    , fa2Address = toAddress fa2Contract
    , startTime = startTime
    , endTime = endTime
    , bondingCurve = 0
    , initialTokenId = 0
    , reserveAddress = reserveAddress
    , profitAddress = profitAddress
    , tokenInfo = mempty 
    }

placeBid :: (HasCallStack, MonadNettest caps base m) => TAddress Auction.AuctionEntrypoints -> Natural -> Mutez -> m ()
placeBid contract numOffers bidAmount =
  transfer TransferData
    { tdTo = contract
    , tdAmount = bidAmount
    , tdEntrypoint = ep "bid"
    , tdParameter = Auction.BidParam 0 numOffers bidAmount
    }

addSampleBondingCurve :: (HasCallStack, MonadNettest caps base m) => TAddress Auction.AuctionEntrypoints -> m ()
addSampleBondingCurve = addBondingCurve sampleBondingCurve sampleIntegralCurve

addBondingCurve :: (HasCallStack, MonadNettest caps base m) => ('[Natural] L.:-> '[Mutez])  -> ('[Natural] L.:-> '[Mutez])-> TAddress Auction.AuctionEntrypoints -> m ()
addBondingCurve bc integral contract = do
  call contract (Call @"Add_bonding_curve") (bc, integral) 