module Test.EnglishAuction.TezFixedFee where

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
import Lorentz.Contracts.EnglishAuction.TezFixedFee
import Lorentz.Contracts.MinterSdk
import qualified Lorentz.Contracts.NoAllowlist as NoAllowlist
import Lorentz.Contracts.Spec.FA2Interface (TokenId(..))
import Michelson.Typed (convertContract, untypeValue)
import Morley.Nettest
import Test.Util (balanceOf, clevelandProp, genMutez', iterateM)
import Tezos.Core (timestampPlusSeconds)

hprop_Auctions_must_start_within_the_configured_number_of_seconds :: Property
hprop_Auctions_must_start_within_the_configured_number_of_seconds =
  property $ do
    testData@TestData{testMaxConfigToStartTime} <- forAll genTestData

    let timeToStartLowerBound = toInteger testMaxConfigToStartTime + 1
    testTimeToStart <- forAll $ Gen.integral $ Range.linear timeToStartLowerBound (timeToStartLowerBound * 2)
    let testData' = testData { testTimeToStart = testTimeToStart }

    clevelandProp $ do
      setup@Setup{contract} <- testSetup testData'

      configureAuction testData' setup
        `expectFailure`
          failedWith contract [mt|MAX_CONFIG_TO_START_TIME_VIOLATED|]

hprop_First_bid_is_valid_IFF_it_meets_opening_price :: Property
hprop_First_bid_is_valid_IFF_it_meets_opening_price =
  property $ do

    testData@TestData{testOpeningPrice} <- forAll genTestData
    firstBid <- forAll $ genMutez' (Range.linear 0 (testOpeningPrice * 2))

    clevelandProp $ do
      setup@Setup{seller, startTime, contract} <- testSetup testData
      bidder <- newAddress "bidder"
      withSender seller $ configureAuction testData setup
      waitForAuctionToStart testData

      withSender bidder $
        if firstBid >= testOpeningPrice
          then placeBid contract firstBid
          else placeBid contract firstBid `expectFailure` failedWith contract
                 ([mt|INVALID_BID_AMOUNT|], (((testOpeningPrice, firstBid), seller, startTime), startTime))

hprop_Subsequent_bids_are_valid_if_they_are_above_'min_raise' :: Property
hprop_Subsequent_bids_are_valid_if_they_are_above_'min_raise' =
  property $ do
    testData@TestData{testOpeningPrice} <- forAll genTestData

    firstBid <- forAll $ genBid testData testOpeningPrice
    let minRaisePercentOfFirstBid = minRaisePercentOfBid testData firstBid

    -- Generate a `min_raise` with a 50% chance of it being above `min_raise_percent * firstBid`
    -- and 50% of it being below.
    testMinRaise <- forAll $ do
      (lowerBound, upperBound) <-
        Gen.element
          [ (1, minRaisePercentOfFirstBid)
          , (minRaisePercentOfFirstBid, minRaisePercentOfFirstBid * 2)
          ]
      genMutez' (Range.linear lowerBound upperBound)

    let testData' = testData { testMinRaise = testMinRaise }

    let secondBidLowerBound = firstBid + testMinRaise
    let secondBidUpperBound = firstBid + (testMinRaise * 2)
    secondBid <- forAll $ genMutez' (Range.linear secondBidLowerBound secondBidUpperBound)

    -- These generators have been carefully calibrated to ensure `secondBid` is not ALWAYS
    -- above BOTH `min_raise` and `min_raise_percent`.
    -- For this property to be meaningful, we want there to be a fair chance of `secondBid`
    -- being above `min_raise`, but below `min_raise_percent`.
    when (secondBid < firstBid + minRaisePercentOfFirstBid) $
      label "Second bid is above `min_raise`, but below `min_raise_percent`"

    clevelandProp $ do
      setup@Setup{seller, contract} <- testSetup testData'
      bidder1 <- newAddress "bidder-1"
      bidder2 <- newAddress "bidder-2"

      withSender seller $ configureAuction testData' setup
      waitForAuctionToStart testData'

      withSender bidder1 $ placeBid contract firstBid
      withSender bidder2 $ placeBid contract secondBid

hprop_Subsequent_bids_are_valid_if_they_are_above_'min_raise_percent' :: Property
hprop_Subsequent_bids_are_valid_if_they_are_above_'min_raise_percent' =
  property $ do
    testData@TestData{testOpeningPrice} <- forAll genTestData

    firstBid <- forAll $ genBid testData testOpeningPrice
    let minRaisePercentOfFirstBid = minRaisePercentOfBid testData firstBid

    -- Generate a `min_raise` with a 50% chance of it being above `min_raise_percent * firstBid`
    -- and 50% of it being below.
    testMinRaise <- forAll $ do
      (lowerBound, upperBound) <-
        Gen.element
          [ (1, minRaisePercentOfFirstBid)
          , (minRaisePercentOfFirstBid, minRaisePercentOfFirstBid * 2)
          ]
      genMutez' (Range.linear lowerBound upperBound)

    let testData' = testData { testMinRaise = testMinRaise }

    let secondBidLowerBound = firstBid + minRaisePercentOfFirstBid
    let secondBidUpperBound = firstBid + (minRaisePercentOfFirstBid * 2)
    secondBid <- forAll $ genMutez' (Range.linear secondBidLowerBound secondBidUpperBound)

    -- These generators have been carefully calibrated to ensure `secondBid` is not ALWAYS
    -- above BOTH `min_raise` and `min_raise_percent`.
    -- For this property to be meaningful, we want there to be a fair chance of `secondBid`
    -- being above `min_raise_percent`, but below `min_raise`.
    when (secondBid < firstBid + testMinRaise) $
      label "Second bid is above `min_raise_percent`, but below `min_raise`"

    clevelandProp $ do
      setup@Setup{seller, contract} <- testSetup testData'
      bidder1 <- newAddress "bidder-1"
      bidder2 <- newAddress "bidder-2"

      withSender seller $ configureAuction testData' setup
      waitForAuctionToStart testData'

      withSender bidder1 $ placeBid contract firstBid
      withSender bidder2 $ placeBid contract secondBid

hprop_Subsequent_bids_are_invalid_if_they_are_below_'min_raise'_and_'min_raise_percent' :: Property
hprop_Subsequent_bids_are_invalid_if_they_are_below_'min_raise'_and_'min_raise_percent' =
  property $ do
    testData@TestData{testOpeningPrice} <- forAll genTestData

    firstBid <- forAll $ genBid testData testOpeningPrice

    -- Make sure `min_raise` and `min_raise_percent` are at least 1,
    -- and generate a second bid below both thresholds.
    testMinRaise <- forAll $ Gen.integral (Range.linear 1 1000)
    testMinRaisePercent <- forAll $ Gen.integral (Range.linear 1 100)
    let testData' = testData { testMinRaise = testMinRaise, testMinRaisePercent = testMinRaisePercent }

    let minRaisePercentOfFirstBid = minRaisePercentOfBid testData' firstBid
    let secondBidLowerBound = firstBid
    let secondBidUpperBound = firstBid + (testMinRaise `min` minRaisePercentOfFirstBid) - 1
    secondBid <- forAll $ genMutez' (Range.linear secondBidLowerBound secondBidUpperBound)

    clevelandProp $ do
      setup@Setup{seller, contract, startTime} <- testSetup testData'
      bidder1 <- newAddress "bidder-1"
      bidder2 <- newAddress "bidder-2"
      withSender seller $ configureAuction testData' setup
      waitForAuctionToStart testData'

      withSender bidder1 $
        placeBid contract firstBid

      withSender bidder2 $ do
        placeBid contract secondBid `expectFailure` failedWith contract
          ([mt|INVALID_BID_AMOUNT|], ((firstBid, secondBid) , bidder1, startTime), startTime)

hprop_Subsequent_bids_must_be_greater_than_last_bid :: Property
hprop_Subsequent_bids_must_be_greater_than_last_bid =
  property $ do
    testData@TestData{testOpeningPrice} <- forAll genTestData
    firstBid <- forAll $ genBid testData testOpeningPrice
    secondBid <- forAll $ genMutez' (Range.linearFrom firstBid 0 firstBid)

    clevelandProp $ do
      setup@Setup{seller, contract, startTime} <- testSetup testData
      bidder1 <- newAddress "bidder-1"
      bidder2 <- newAddress "bidder-2"
      withSender seller $ configureAuction testData setup
      waitForAuctionToStart testData

      withSender bidder1 $
        placeBid contract firstBid

      withSender bidder2 $ do
        placeBid contract secondBid `expectFailure` failedWith contract
          ([mt|INVALID_BID_AMOUNT|], ((firstBid, secondBid) , bidder1, startTime), startTime)

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
      withSender seller $ configureAuction testData' setup
      waitForAuctionToStart testData'

      let
        placeBids [] = pass
        placeBids (((bid, bidder), waitingTime) : rest) = do
          advanceTime (sec $ fromIntegral waitingTime)

          if waitingTime <= testRoundTime
            then do
              withSender bidder $ do
                placeBid contract bid

              -- `resolve` should fail because auction is still in progress.
              resolveAuction contract `expectFailure` failedWith contract [mt|AUCTION_NOT_ENDED|]

              placeBids rest

            else do
              withSender bidder $ do
                placeBid contract bid `expectFailure` failedWith contract [mt|NOT_IN_PROGRESS|]

              -- `resolve` should succeed.
              resolveAuction contract

      placeBids (toList bids `zip` toList bidders `zip` toList bidWaitingTimes)

hprop_Auction_ends_after_'end_time' :: Property
hprop_Auction_ends_after_'end_time' =
  property $ do
    testData@TestData{testAuctionDuration, testOpeningPrice} <- forAll genTestData
    firstBid <- forAll $ genBid testData testOpeningPrice

    clevelandProp $ do
      setup@Setup{seller, contract} <- testSetup testData
      withSender seller $ configureAuction testData setup

      waitForAuctionToStart testData
      advanceTime (sec $ fromIntegral testAuctionDuration)

      -- `bid` should fail.
      placeBid contract firstBid `expectFailure` failedWith contract [mt|NOT_IN_PROGRESS|]

      -- `resolve` should succeed.
      resolveAuction contract

hprop_Placing_a_bid_within_'extend_time'_seconds_of_'end_time'_extends_the_auction_duration :: Property
hprop_Placing_a_bid_within_'extend_time'_seconds_of_'end_time'_extends_the_auction_duration =
  property $ do
    testData@TestData{testExtendTime, testAuctionDuration} <- forAll genTestData
    let testData' = testData { testRoundTime = testAuctionDuration `max` testExtendTime }
    bids <- forAll $ genSomeBids testData'

    bidWaitingTimes <- forAll $ forM [1 .. length bids] \i ->
      -- The first time we place a bid, when `extendTime` > `auctionDuration`,
      -- if we waited for `extendTime` seconds, then the auction would terminate
      -- before we could place our first bid.
      -- So, in this case, we can only wait for at most `auctionDuration` seconds.
      if i == 1 && testExtendTime > testAuctionDuration
        then Gen.integral (Range.linear 0 (testAuctionDuration - 1))
        else Gen.integral (Range.linear 0 (testExtendTime - 1))

    clevelandProp $ do
      setup@Setup{seller, contract} <- testSetup testData'
      bidders <- mkBidders bids
      withSender seller $ configureAuction testData' setup
      waitForAuctionToStart testData'

      -- Wait long enough to be within the `extend_time` window.
      if testExtendTime > testAuctionDuration
        then pass
        else advanceTime (sec $ fromIntegral (testAuctionDuration - testExtendTime))

      forM_ (toList bids `zip` toList bidders `zip` bidWaitingTimes) \((bid, bidder), waitingTime) -> do
        advanceTime (sec $ fromIntegral waitingTime)

        t0 <- getNow
        withSender bidder $ do
          placeBid contract bid
        t1 <- getEndTime contract

        t1 @== t0 `timestampPlusSeconds` fromIntegral testExtendTime

        -- `resolve` should fail because auction is still in progress.
        resolveAuction contract `expectFailure` failedWith contract [mt|AUCTION_NOT_ENDED|]
  where
    getEndTime contract =
      getStorage' contract
        <&> fromVal @AuctionStorage
        >>= getAuction (AuctionId 0)
        <&> \a -> endTime (a :: Auction)

hprop_Placing_a_bid_refunds_the_previous_bid_to_the_respective_bidder :: Property
hprop_Placing_a_bid_refunds_the_previous_bid_to_the_respective_bidder =
  property $ do
    testData <- forAll genTestData
    bids <- forAll $ genSomeBids testData

    clevelandProp $ do
      setup@Setup{seller, contract} <- testSetup testData
      bidders <- mkBidders bids
      withSender seller $ configureAuction testData setup
      waitForAuctionToStart testData

      biddersInitialBalances <- forM bidders getBalance

      forM_ (toList bids `zip` toList bidders) \(bid, bidder) -> do
        withSender bidder $
          placeBid contract bid

      biddersFinalBalances <- forM bidders getBalance

      -- The balance of every bidder (except the last) should have been restored.
      init biddersFinalBalances @== init biddersInitialBalances

hprop_Assets_are_held_in_escrow :: Property
hprop_Assets_are_held_in_escrow =
  property $ do
    testData@TestData{testTokenBatches} <- forAll genTestData

    clevelandProp $ do
      setup@Setup{seller, contract, fa2Contracts} <- testSetup testData
      withSender seller $ configureAuction testData setup
      waitForAuctionToStart testData

      -- All the tokens should have been transferred to the contract.
      forM_ (testTokenBatches `zip` fa2Contracts) \(tokenBatch, fa2Contract) -> do
        let expectedBalances =
              tokenBatch
                <&> (\FA2Token{tokenId, amount} -> (tokenId, amount))
                & Map.fromListWith (+)
        forM_ (Map.toList expectedBalances) \(tokenId, expectedBalance) -> do
          sellerBalance <- balanceOf fa2Contract tokenId seller
          sellerBalance @== 0
          contractBalance <- balanceOf fa2Contract tokenId contract
          contractBalance @== expectedBalance

hprop_Assets_and_bid_are_returned_upon_cancellation :: Property
hprop_Assets_and_bid_are_returned_upon_cancellation =
  property $ do
    testData@TestData{testTokenBatches} <- forAll genTestData
    bids <- forAll $ genManyBids testData

    clevelandProp $ do
      setup@Setup{seller, contract, fa2Contracts} <- testSetup testData
      bidders <- mkBidders bids
      sellerInitialTez <- getBalance seller
      withSender seller $ configureAuction testData setup
      waitForAuctionToStart testData

      biddersInitialBalances <- forM bidders getBalance

      forM_ (bids `zip` bidders) \(bid, bidder) -> do
        withSender bidder $
          placeBid contract bid

      withSender seller $
        cancelAuction contract

      biddersFinalBalances <- forM bidders getBalance

      -- The balance of every bidder should have been restored.
      biddersFinalBalances @== biddersInitialBalances

      -- All the tokens should have been transferred back to the seller.
      forM_ (testTokenBatches `zip` fa2Contracts) \(tokenBatch, fa2Contract) -> do
        let expectedBalances =
              tokenBatch
                <&> (\FA2Token{tokenId, amount} -> (tokenId, amount))
                & Map.fromListWith (+)
        forM_ (Map.toList expectedBalances) \(tokenId, expectedBalance) -> do
          sellerBalance <- balanceOf fa2Contract tokenId seller
          sellerBalance @== expectedBalance
          contractBalance <- balanceOf fa2Contract tokenId contract
          contractBalance @== 0

      sellerFinalTez <- getBalance seller
      sellerFinalTez @== sellerInitialTez

hprop_Assets_are_returned_to_seller_when_auction_ends_with_no_bids :: Property
hprop_Assets_are_returned_to_seller_when_auction_ends_with_no_bids =
  property $ do
    testData@TestData{testAuctionDuration, testTokenBatches} <- forAll genTestData

    clevelandProp $ do
      setup@Setup{seller, contract, fa2Contracts} <- testSetup testData
      sellerInitialTez <- getBalance seller
      withSender seller $ configureAuction testData setup

      waitForAuctionToStart testData
      advanceTime (sec $ fromIntegral testAuctionDuration)
      resolveAuction contract

      -- All the tokens should have been transferred back to the seller.
      forM_ (testTokenBatches `zip` fa2Contracts) \(tokenBatch, fa2Contract) -> do
        let expectedBalances =
              tokenBatch
                <&> (\FA2Token{tokenId, amount} -> (tokenId, amount))
                & Map.fromListWith (+)
        forM_ (Map.toList expectedBalances) \(tokenId, expectedBalance) -> do
          sellerBalance <- balanceOf fa2Contract tokenId seller
          sellerBalance @== expectedBalance
          contractBalance <- balanceOf fa2Contract tokenId contract
          contractBalance @== 0

      sellerFinalTez <- getBalance seller
      sellerFinalTez @== sellerInitialTez

hprop_Assets_are_transferred_to_highest_bidder :: Property
hprop_Assets_are_transferred_to_highest_bidder =
  property $ do
    testData@TestData{testExtendTime, testAuctionDuration, testTokenBatches} <- forAll genTestData
    bids <- forAll $ genSomeBids testData

    clevelandProp $ do
      setup@Setup{seller, contract, fa2Contracts} <- testSetup testData
      bidders <- mkBidders bids
      withSender seller $ configureAuction testData setup

      -- Wait for the auction to start.
      waitForAuctionToStart testData

      forM_ (toList bids `zip` toList bidders) \(bid, bidder) -> do
        withSender bidder $
          placeBid contract bid

      -- Wait for the auction to end.
      advanceTime (sec $ fromIntegral $ testAuctionDuration `max` testExtendTime)

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

hprop_Winning_bid_is_transferred_to_seller_and_fee_to_fee_collector :: Property
hprop_Winning_bid_is_transferred_to_seller_and_fee_to_fee_collector =
  property $ do
    testData@TestData{testExtendTime, testAuctionDuration, testFeePercent} <- forAll genTestData
    bids <- forAll $ genSomeBids testData

    clevelandProp $ do
      setup@Setup{seller, feeCollector, contract} <- testSetup testData
      bidders <- mkBidders bids

      sellerInitialTez <- getBalance seller
      feeCollectorInitialTez <- getBalance feeCollector

      withSender seller $ configureAuction testData setup

      -- Wait for the auction to start.
      waitForAuctionToStart testData

      forM_ (toList bids `zip` toList bidders) \(bid, bidder) -> do
        withSender bidder $
          placeBid contract bid

      -- Wait for the auction to end.
      advanceTime (sec $ fromIntegral $ testAuctionDuration `max` testExtendTime)

      resolveAuction contract

      let winningBid = last bids
      let expectedFee = (winningBid * fromIntegral testFeePercent) `div` 100

      sellerFinalTez <- getBalance seller
      sellerFinalTez @== sellerInitialTez + winningBid - expectedFee
      feeCollectorFinalTez <- getBalance feeCollector
      feeCollectorFinalTez @== feeCollectorInitialTez + expectedFee

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

data Setup = Setup
  { contract :: TAddress (AuctionEntrypoints NoAllowlist.Entrypoints)
  , fa2Contracts :: [TAddress FA2.FA2SampleParameter]
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
    originateAuctionContract AuctionStorage
      { pausableAdmin = Nothing
      , currentId = 0
      , maxAuctionTime = testMaxAuctionTime testData
      , maxConfigToStartTime = testMaxConfigToStartTime testData
      , auctions = mempty
      , allowlist = ()
      , fee = FeeData feeCollector (testFeePercent testData)
      }

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

  now <- getNow
  let startTime = now `timestampPlusSeconds` testTimeToStart testData
  let endTime = startTime `timestampPlusSeconds` fromIntegral (testAuctionDuration testData)

  pure Setup {..}

waitForAuctionToStart :: (HasCallStack, MonadNettest caps base m) => TestData -> m ()
waitForAuctionToStart TestData{testTimeToStart} =
  advanceTime (sec $ fromIntegral testTimeToStart)

originateAuctionContract :: MonadNettest caps base m => AuctionStorage -> m (TAddress (AuctionEntrypoints NoAllowlist.Entrypoints))
originateAuctionContract storage = do
  TAddress @(AuctionEntrypoints NoAllowlist.Entrypoints) <$> originateUntypedSimple "auction-tez-fixed-fee"
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

----------------------------------------------------------------------------
-- Call entrypoints
----------------------------------------------------------------------------

configureAuction :: (HasCallStack, MonadNettest caps base m) => TestData -> Setup -> m ()
configureAuction testData Setup{fa2Contracts, contract, startTime, endTime}  = do
  let assets =
        (testTokenBatches testData `zip` fa2Contracts) <&> \(tokenBatch, fa2Contract) ->
          Tokens
            { fa2Address = toAddress fa2Contract
            , fa2Batch = tokenBatch
            }

  call contract (Call @"Configure") ConfigureParam
    { openingPrice = testOpeningPrice testData
    , minRaisePercent = testMinRaisePercent testData
    , minRaise = testMinRaise testData
    , roundTime = testRoundTime testData
    , extendTime = testExtendTime testData
    , asset = assets
    , startTime = startTime
    , endTime = endTime
    }

placeBid :: (HasCallStack, MonadNettest caps base m) => TAddress (AuctionEntrypoints NoAllowlist.Entrypoints) -> Mutez -> m ()
placeBid contract bidAmount =
  transfer TransferData
    { tdTo = contract
    , tdAmount = bidAmount
    , tdEntrypoint = ep "bid"
    , tdParameter = AuctionId 0
    }

cancelAuction :: (HasCallStack, MonadNettest caps base m) => TAddress (AuctionEntrypoints NoAllowlist.Entrypoints) -> m ()
cancelAuction contract =
  call contract (Call @"Cancel") (AuctionId 0)

resolveAuction :: (HasCallStack, MonadNettest caps base m) => TAddress (AuctionEntrypoints NoAllowlist.Entrypoints) -> m ()
resolveAuction contract =
  call contract (Call @"Resolve") (AuctionId 0)
