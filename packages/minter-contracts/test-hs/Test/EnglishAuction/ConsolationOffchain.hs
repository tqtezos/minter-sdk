module Test.EnglishAuction.ConsolationOffchain where

import Cleveland.Util (sec)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Traversable.WithIndex (TraversableWithIndex, ifor)
import Fmt ((+|), (|+))
import Hedgehog (Gen, Property, forAll, label, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Indigo.Contracts.FA2Sample as FA2
import Lorentz hiding (amount, contract, now)
import Lorentz.Contracts.EnglishAuction.Common
import Lorentz.Contracts.EnglishAuction.TezFixedFee hiding (testSetup)
import Lorentz.Contracts.MinterSdk
import qualified Lorentz.Contracts.NoAllowlist as NoAllowlist
import Lorentz.Contracts.Spec.FA2Interface (TokenId(..))
import Michelson.Typed (convertContract, untypeValue)
import Morley.Nettest
import Test.Util (balanceOf, clevelandProp, genMutez', iterateM)
import Tezos.Core (timestampPlusSeconds)
import qualified Test.EnglishAuction.TezFixedFee as Tez
import qualified Lorentz.Contracts.EnglishAuction.ConsolationOffchain as ConsolationOffchain
import Test.EnglishAuction.Util
import qualified Lorentz.Contracts.EnglishAuction.Consolation as Consolation
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2I

hprop_Assets_are_transferred_to_highest_bidder_after_consolation_tokens_sent :: Property
hprop_Assets_are_transferred_to_highest_bidder_after_consolation_tokens_sent =
  property $ do
    testData@TestData{testExtendTime, testAuctionDuration, testTokenBatches, testMaxConsolationWinners} <- forAll genTestData
    bids <- forAll $ genSomeBids testData

    clevelandProp $ do
      setup@Setup{seller, contract, fa2Contracts} <- testSetup testData
      bidders <- mkBidders bids
      let numBids = length (toList bids) 
      withSender seller $ configureAuction testData setup

      -- Wait for the auction to start.
      waitForAuctionToStart testData

      forM_ (toList bids `zip` toList bidders) \(bid, bidder) -> do
        withSender bidder $
          placeBid contract bid

      -- Wait for the auction to end.
      advanceTime (sec $ fromIntegral $ testAuctionDuration `max` testExtendTime)
      
      withSender seller $ 
        sendConsolation (genConsolationWinners (fromIntegral numBids) (fromIntegral testMaxConsolationWinners)) contract

      withSender seller $
        resolveAuction contract

      let winner = last bidders

      -- All the tokens should have been transferred to the winner.
      forM_ (testTokenBatches `zip` fa2Contracts) \(tokenBatch, fa2Contract) -> do
        let expectedBalances =
              tokenBatch
                <&> (\FA2Token{tokenId, amount} -> (tokenId, amount))
                & Map.fromListWith (+)
        forM_ (Map.toList expectedBalances) \(tokenId, expectedBalance) -> do
          sellerBalance <- balanceOf fa2Contract tokenId seller
          sellerBalance @== 0
          contractBalance <- balanceOf fa2Contract tokenId contract
          contractBalance @== 0
          winnerBalance <- balanceOf fa2Contract tokenId winner
          winnerBalance @== expectedBalance

hprop_Resolve_auction_fails_if_not_all_consolation_tokens_sent :: Property
hprop_Resolve_auction_fails_if_not_all_consolation_tokens_sent =
  property $ do
    testData@TestData{testExtendTime, testAuctionDuration, testTokenBatches, testMaxConsolationWinners} <- forAll genTestData
    bids <- forAll $ genMultipleBids testData

    clevelandProp $ do
      setup@Setup{seller, contract, fa2Contracts} <- testSetup testData
      bidders <- mkBidders bids
      let numBids = length (toList bids)
      -- Ensures testMaxConsolationWinners is at least 1  
      withSender seller $ configureAuction (testData {testMaxConsolationWinners = testMaxConsolationWinners + 1}) setup

      -- Wait for the auction to start.
      waitForAuctionToStart testData

      forM_ (toList bids `zip` toList bidders) \(bid, bidder) -> do
        withSender bidder $
          placeBid contract bid

      -- Wait for the auction to end.
      advanceTime (sec $ fromIntegral $ testAuctionDuration `max` testExtendTime)
      
      withSender seller $ 
        sendConsolation (genIncompleteConsolationWinners (fromIntegral numBids) (fromIntegral testMaxConsolationWinners + 1)) contract

      withSender seller
        (resolveAuction contract `expectFailure` failedWith contract [mt|CONSOLATION_NOT_SENT|])


----------------------------------------------------------------------------
-- Generators
----------------------------------------------------------------------------

data TestData = TestData
  { testFeePercent :: Natural
  , testRoundTime :: Natural
  , testExtendTime :: Natural
  , testMaxConfigToStartTime :: Natural
  , testTimeToStart :: Integer
  , testAuctionDuration :: Natural
  , testMaxAuctionTime :: Natural

  , testOpeningPrice :: Mutez
  , testFirstBid :: Mutez
  , testMinRaise :: Mutez
  , testMinRaisePercent :: Natural

  , testTokenBatches :: [[FA2Token]]
  , testMaxConsolationWinners :: Natural
  }
  deriving stock (Show)

genTestData :: Gen TestData
genTestData = do
  testFeePercent <- Gen.integral (Range.linear 0 100)
  testRoundTime <- Gen.integral (Range.linear 1 100)
  testExtendTime <- Gen.integral (Range.linear 1 100)

  testMaxConfigToStartTime <- Gen.integral (Range.linear 0 100)
  testTimeToStart <- Gen.integral (Range.linear 0 (toInteger testMaxConfigToStartTime))

  testOpeningPrice <- genMutez' (Range.linear 1 1000)
  testFirstBid <- genMutez' (Range.linear testOpeningPrice (testOpeningPrice + 1000))

  testMinRaise <- genMutez' (Range.linear 1 1000)
  testMinRaisePercent <- Gen.integral (Range.linear 1 100)

  testMaxConsolationWinners <- Gen.integral (Range.linear 0 100)

  testAuctionDuration <- Gen.integral (Range.linear 1 100)
  testMaxAuctionTime <- Gen.integral (Range.linear testAuctionDuration (testAuctionDuration * 2))

  testTokenBatches <-
    Gen.list (Range.linear 0 10) $
        Gen.list (Range.linear 0 10) $
        FA2Token
          <$> (TokenId <$> Gen.integral (Range.linear 0 10))
          <*> Gen.integral (Range.linear 0 10)

  pure $ TestData {..}

-- | Generates a list with 1 or more valid bids
genSomeBids :: TestData -> Gen (NonEmpty Mutez)
genSomeBids testData = do
  len <- Gen.int (Range.linear 0 5)
  firstBid <- genBid testData (testOpeningPrice testData)
  moreBids <- iterateM len (genBid testData) firstBid
  pure $ firstBid :| moreBids

-- | Generates a list with 2 or more valid bids
genMultipleBids :: TestData -> Gen (NonEmpty Mutez)
genMultipleBids testData = do
  len <- Gen.int (Range.linear 1 5)
  firstBid <- genBid testData (testOpeningPrice testData)
  moreBids <- iterateM len (genBid testData) firstBid
  pure $ firstBid :| moreBids

-- | Generates a list with 0 or more valid bids
genManyBids :: TestData -> Gen [Mutez]
genManyBids testData = do
  len <- Gen.int (Range.linear 0 5)
  iterateM len (genBid testData) (testOpeningPrice testData)

-- | Generate a valid bid, such that it is:
--
-- * Greater than the previous bid or opening price.
-- * Above either the `min_raise` or the `min_raise_percent` thresholds.
genBid :: TestData -> Mutez -> Gen Mutez
genBid testData@TestData{testMinRaise} previousBid = do
  raiseLowerBound <- Gen.element [testMinRaise, minRaisePercentOfBid testData previousBid]
  raise <- genMutez' (Range.linear raiseLowerBound (raiseLowerBound + 1000))
  pure $ previousBid + raise

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

tailSafe :: [a] -> [a]
tailSafe [] = [] 
tailSafe (x:xs) = xs 

data Setup = Setup
  { contract :: TAddress (ConsolationOffchain.AuctionEntrypoints NoAllowlist.Entrypoints)
  , fa2Contracts :: [TAddress FA2.FA2SampleParameter]
  , consolationFa2Contract :: TAddress FA2.FA2SampleParameter 
  , feeCollector :: Address
  , seller :: Address
  , startTime :: Timestamp
  , endTime :: Timestamp
  }

testSetup :: MonadNettest caps base m => TestData -> m Setup
testSetup testData = do
  feeCollector <- newAddress "fee-collector"
  seller <- newAddress "seller"

  contract <-
    originateAuctionTezOffchainConsolation seller

  -- The FA2 contracts with the seller's assets
  fa2Contracts <- forM (testTokenBatches testData) \tokenBatch -> do
    let tokenIds = List.nub $ tokenId <$> tokenBatch
    let ledger =
          Map.fromListWith (+) $
            tokenBatch <&> \FA2Token{tokenId, amount} -> ((seller, tokenId), amount)

    originateSimple "asset-fa2"
      FA2.Storage
      { sLedger = BigMap ledger
      , sOperators = BigMap $ Map.fromList [((seller, toAddress contract), ())]
      , sTokenMetadata = mempty
      }
      (FA2.fa2Contract def { FA2.cAllowedTokenIds = tokenIds })

  -- The FA2 contract with the consolation token
  consolationFa2Contract <- do
    let ledger =
          Map.fromList $
             [((seller, FA2I.TokenId 0), 1000)]

    originateSimple "consolation-fa2"
      FA2.Storage
      { sLedger = BigMap ledger
      , sOperators = BigMap $ Map.fromList [((seller, toAddress contract), ())]
      , sTokenMetadata = mempty
      }
      (FA2.fa2Contract def { FA2.cAllowedTokenIds = [FA2I.TokenId 0] })

  now <- getNow
  let startTime = now `timestampPlusSeconds` testTimeToStart testData
  let endTime = startTime `timestampPlusSeconds` fromIntegral (testAuctionDuration testData)

  pure Setup {..}


waitForAuctionToStart :: (HasCallStack, MonadNettest caps base m) => TestData -> m ()
waitForAuctionToStart TestData{testTimeToStart} =
  advanceTime (sec $ fromIntegral testTimeToStart)

originateAuctionContract :: MonadNettest caps base m => AuctionStorage -> m (TAddress (ConsolationOffchain.AuctionEntrypoints NoAllowlist.Entrypoints))
originateAuctionContract storage = do
  TAddress @(ConsolationOffchain.AuctionEntrypoints NoAllowlist.Entrypoints) <$> originateUntypedSimple "auction-tez-fixed-fee"
    (untypeValue $ toVal storage)
    (convertContract englishAuctionTezFixedFeeContract)

mkBidders :: (MonadNettest caps base m, TraversableWithIndex Int f) => f Mutez -> m (f Address)
mkBidders bids =
  ifor bids \i _ -> newAddress ("bidder-" <> show i)

getAuction :: MonadNettest caps base m => AuctionId -> AuctionStorage -> m Auction
getAuction auctionId st = do
  let auctionOpt =
        st
          & auctions
          & unBigMap
          & Map.lookup auctionId
  case auctionOpt of
    Just auction -> pure auction
    Nothing -> failure $ "Expected the storage to contain an auction with ID '" +| auctionId |+ "', but it didn't."

-- | Calculates the `min_raise_percent` of a bid.
minRaisePercentOfBid :: TestData -> Mutez -> Mutez
minRaisePercentOfBid TestData{testMinRaisePercent} bid =
  ceiling @Double @Mutez ((fromIntegral testMinRaisePercent * fromIntegral bid) / 100)

genConsolationWinners :: Natural -> Natural -> [Natural]
genConsolationWinners numBids maxConsolationWinners 
    | numBids <= 1 || maxConsolationWinners == 0 = [] 
    | maxConsolationWinners + 1 >= numBids = [0 .. (numBids - 2)]
    | otherwise = [ (numBids - maxConsolationWinners - 1) .. (numBids - 2)]

genIncompleteConsolationWinners :: Natural -> Natural -> [Natural]
genIncompleteConsolationWinners numBids maxConsolationWinners = tailSafe $ genConsolationWinners numBids maxConsolationWinners

----------------------------------------------------------------------------
-- Call entrypoints
----------------------------------------------------------------------------

configureAuction :: (HasCallStack, MonadNettest caps base m) => TestData -> Setup -> m ()
configureAuction testData Setup{fa2Contracts, contract, startTime, endTime, consolationFa2Contract}  = do
  let assets =
        (testTokenBatches testData `zip` fa2Contracts) <&> \(tokenBatch, fa2Contract) ->
          Tokens
            { fa2Address = toAddress fa2Contract
            , fa2Batch = tokenBatch
            }

  call contract (Call @"Configure") ConsolationOffchain.ConfigureParam
    { openingPrice = testOpeningPrice testData
    , minRaisePercent = testMinRaisePercent testData
    , minRaise = testMinRaise testData
    , roundTime = testRoundTime testData
    , extendTime = testExtendTime testData
    , asset = assets
    , startTime = startTime
    , endTime = endTime
    , consolationToken = Consolation.GlobalTokenId (toAddress consolationFa2Contract) (FA2I.TokenId 0)
    , maxConsolationWinners = testMaxConsolationWinners testData
    }

placeBid :: (HasCallStack, MonadNettest caps base m) => TAddress (ConsolationOffchain.AuctionEntrypoints NoAllowlist.Entrypoints) -> Mutez -> m ()
placeBid contract bidAmount =
  transfer TransferData
    { tdTo = contract
    , tdAmount = bidAmount
    , tdEntrypoint = ep "bid"
    , tdParameter = AuctionId 0
    }

cancelAuction :: (HasCallStack, MonadNettest caps base m) => TAddress (ConsolationOffchain.AuctionEntrypoints NoAllowlist.Entrypoints) -> m ()
cancelAuction contract =
  call contract (Call @"Cancel") (AuctionId 0)

resolveAuction :: (HasCallStack, MonadNettest caps base m) => TAddress (ConsolationOffchain.AuctionEntrypoints NoAllowlist.Entrypoints) -> m ()
resolveAuction contract =
  call contract (Call @"Resolve") (AuctionId 0)

sendConsolation :: (HasCallStack, MonadNettest caps base m) => [Natural] -> TAddress (ConsolationOffchain.AuctionEntrypoints NoAllowlist.Entrypoints) -> m ()
sendConsolation bidderIds contract =
  call contract (Call @"Send_consolation") (AuctionId 0, bidderIds)