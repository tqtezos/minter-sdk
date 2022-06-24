module Test.MultiunitAuction.Auction where
import Lorentz.Value
import qualified Lorentz as L

import Data.Maybe (fromMaybe)

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
import Tezos.Core (timestampPlusSeconds, unsafeMulMutez)
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
      let bid = Auction.BidParam 0 1 firstBid

      withSender bidder $
        if firstBid >= testPriceFloor
          then placeBid contract bid
          else placeBid contract bid 
               `expectFailure` 
                  failedWith contract
                    ([mt|INVALID_BID_AMOUNT|], (bidder, firstBid, startTime, startTime))

hprop_Auction_ends_if_no_bids_are_placed_within_'round_time'_seconds :: Property
hprop_Auction_ends_if_no_bids_are_placed_within_'round_time'_seconds =
  property $ do
    testData@TestData{testRoundTime} <- forAll genTestData
    bids <- forAll $ genSomeBids testData
    -- Generate a list of times to wait before placing each bid.
    -- Each bid may or may not fall within the `round_time` window.
    bidWaitingTimes <- forAll $ forM bids \_ -> Gen.integral (Range.linear 0 (testRoundTime * 2))

    let auctionDuration = sum bidWaitingTimes + 1
    let testData' = testData { testAuctionDuration = auctionDuration, testMaxAuctionTime = auctionDuration}

    clevelandProp $ do
      setup@Setup{seller, contract} <- testSetup testData'
      bidders <- mkBidders bids
      withSender seller $ addSampleBondingCurve contract
      withSender seller $ configureAuction testData' setup
      waitForAuctionToStart testData'
      
      let
        placeBids ([], _) = pass
        placeBids ((((bid, bidder), waitingTime) : rest), successfulBids) = do
          advanceTime (sec $ fromIntegral waitingTime)

          if waitingTime <= testRoundTime
            then do
              withSender bidder $ do
                placeBid contract bid

              -- `resolve` should fail because auction is still in progress.
              resolveAuction contract `expectFailure` failedWith contract [mt|AUCTION_NOT_ENDED|]

              placeBids (rest, successfulBids + 1)

            else do
              withSender bidder $ do
                placeBid contract bid `expectFailure` failedWith contract [mt|NOT_IN_PROGRESS|]
              
              -- return bids, max 6 bids
              if successfulBids > 0
                then do 
                  returnBids contract (Auction.AuctionId 0, 6) --max bids
                else do 
                  pass
              
              --auctionStorage <- fromVal @Auction.AuctionStorage <$> getStorage' contract
              --heapSize <- getHeapSize 0 auctionStorage

              --if heapSize > 0 
              --  then do 
              --    returnOffers contract (Auction.AuctionId 0, 1000) --max offers in a bid
              --  else do 
              --    pass
              
              -- `resolve` should succeed.
              resolveAuction contract

      placeBids (toList bids `zip` toList bidders `zip` toList bidWaitingTimes, 0)
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

-- | Generates a list with 1 or more valid bids
genSomeBids :: TestData -> Gen (NonEmpty Auction.BidParam)
genSomeBids testData = do
  len <- Gen.integral (Range.linear 0 5)
  firstBid <- genBid testData 0
  moreBids <- iterateM len (\_ -> genBid testData 0) firstBid
  pure $ firstBid :| moreBids

-- | Generate a valid bid, such that it is greater than the opening price.
genBid :: TestData -> Natural -> Gen Auction.BidParam
genBid testData auctionId = do
  let minBid = testPriceFloor testData
  bidPrice <- genMutez' (Range.linear minBid (minBid + 1_000_000))
  --quantity <- Gen.integral (Range.linear 1 1000)
  pure $ Auction.BidParam auctionId 1 bidPrice 

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

mkBidders :: (MonadNettest caps base m, TraversableWithIndex Int f) => f Auction.BidParam -> m (f Address)
mkBidders bids =
  ifor bids \i _ -> do 
    addr <- newAddress ("bidder-" <> show i)
    transferMoney addr 1_000_000_e6 --transfer sufficient tez to bidders
    pure addr

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

getHeapSize :: MonadNettest caps base m => Natural -> Auction.AuctionStorage -> m Natural
getHeapSize auctionId auctionStorage = do 
  let heapSize = auctionStorage & Auction.heapSizes & unBigMap & Map.lookup (Auction.AuctionId auctionId)
  case heapSize of
      Just hs -> pure hs
      Nothing -> failure $ "Expected the storage to contain an auction with ID '" +| auctionId |+ "', but it didn't."

--maxAcceptedOffersAtPrice :: ('[Natural] L.:-> '[Mutez]) -> Mutez -> Natural -> Natural 
--maxAcceptedOffersAtPrice bondingCurve offerPrice totalOffers = do
--  let maybeMaxAcceptedOffers = find ( (>= offerPrice) . minPriceValidAtQ) [1 .. totalOffers]
--  fromMaybe totalOffers maybeMaxAcceptedOffers
--  where minPriceValidAtQ = \q -> L.exec q bondingCurve 

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

placeBid :: (HasCallStack, MonadNettest caps base m) => TAddress Auction.AuctionEntrypoints -> Auction.BidParam -> m ()
placeBid contract bidParam =
  transfer TransferData
    { tdTo = contract
    , tdAmount = (Auction.priceParam bidParam) `unsafeMulMutez` (Auction.quantityParam bidParam) 
    , tdEntrypoint = ep "bid"
    , tdParameter = bidParam
    }

addSampleBondingCurve :: (HasCallStack, MonadNettest caps base m) => TAddress Auction.AuctionEntrypoints -> m ()
addSampleBondingCurve = addBondingCurve sampleBondingCurve sampleIntegralCurve

addBondingCurve :: (HasCallStack, MonadNettest caps base m) => ('[Natural] L.:-> '[Mutez])  -> ('[Natural] L.:-> '[Mutez])-> TAddress Auction.AuctionEntrypoints -> m ()
addBondingCurve bc integral contract = do
  call contract (Call @"Add_bonding_curve") (bc, integral) 

resolveAuction :: (HasCallStack, MonadNettest caps base m) => TAddress Auction.AuctionEntrypoints -> m ()
resolveAuction contract =
  call contract (Call @"Resolve") (Auction.AuctionId 0)

returnBids :: (HasCallStack, MonadNettest caps base m) => TAddress Auction.AuctionEntrypoints -> (Auction.AuctionId, Natural) -> m ()
returnBids contract =  
  call contract (Call @"Return_old_bids")

returnOffers:: (HasCallStack, MonadNettest caps base m) => TAddress Auction.AuctionEntrypoints -> (Auction.AuctionId, Natural) -> m ()
returnOffers contract =  
  call contract (Call @"Return_old_offers")
  