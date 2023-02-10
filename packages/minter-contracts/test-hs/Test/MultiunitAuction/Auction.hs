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
import qualified Lorentz.Contracts.MinterCollection.Nft.Asset as Nft
import qualified Lorentz.Contracts.MinterCollection.Nft.Token as NftToken
import qualified Lorentz.Contracts.MinterCollection.Nft.Contract as NftContract
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2I
import qualified Lorentz.Contracts.PausableAdminOption as Admin
import qualified Michelson.Typed as T
import Hedgehog.Gen.Tezos.Core (genMutez')

import qualified Lorentz.Contracts.MultiunitAuction.Auction as Auction
import Test.MinterCollection.Util
import Tezos.Core (timestampPlusSeconds, unsafeMulMutez, mulMutez, unsafeAddMutez, unsafeSubMutez, toMutez, prettyTez)
import Fmt ((+|), (|+))
import Test.Util ( iterateM, clevelandProp )

hprop_Cancel_allows_for_returning_all_bids :: Property
hprop_Cancel_allows_for_returning_all_bids =
  property $ do
    testData@TestData{testExtendTime, testAuctionDuration} <- forAll genTestData
    bids <- forAll $ genSomeBids testData

    clevelandProp $ do
      setup@Setup{seller, contract, fa2Contract, profitAddress} <- testSetup testData
      bidders <- mkBidders bids
      sellerBalanceBefore <- getBalance seller
      auctionContractBalanceBefore <- getBalance contract
      profitAddressBalanceBefore <- getBalance profitAddress

      withSender seller $ addSampleBondingCurve contract
      withSender seller $ configureAuction contract fa2Contract testData setup

      -- Wait for the auction to start.
      waitForAuctionToStart testData

      forM_ (toList bids `zip` toList bidders) $ \(bid, bidder) -> do
        withSender bidder $
          placeBid contract bid
      
      let sumBids = sum $ map (\bid -> Auction.priceParam bid `unsafeMulMutez` Auction.quantityParam bid) bids 
      
      contractBalance <- getBalance contract
      
      contractBalance @== sumBids

      -- Wait for the auction to end.
      advanceTime (sec $ fromIntegral $ testAuctionDuration `max` testExtendTime)
      
      withSender seller $ cancelAuction contract 

      returnBids contract (Auction.AuctionId 0, 6) --max bids

      sellerBalanceAfter <- getBalance seller
      auctionContractBalanceAfter <- getBalance contract
      profitAddressBalanceAfter <- getBalance profitAddress

      sellerBalanceBefore @== sellerBalanceAfter
      profitAddressBalanceAfter @== profitAddressBalanceBefore 
      auctionContractBalanceBefore @== auctionContractBalanceAfter

hprop_Fees_are_paid_correctly :: Property
hprop_Fees_are_paid_correctly =
  property $ do
    testData@TestData{testExtendTime, testAuctionDuration} <- forAll genTestData
    bids <- forAll $ genSomeBids testData

    clevelandProp $ do
      setup@Setup{seller, contract, fa2Contract, profitAddress} <- testSetup testData
      bidders <- mkBidders bids
      let winners = winningBids sampleBondingCurve' (toList bids `zip` toList bidders)
      sellerBalanceBefore <- getBalance seller
      auctionContractBalanceBefore <- getBalance contract
      profitAddressBalanceBefore <- getBalance profitAddress

      withSender seller $ addSampleBondingCurve contract
      withSender seller $ configureAuction contract fa2Contract testData setup

      -- Wait for the auction to start.
      waitForAuctionToStart testData

      forM_ (toList bids `zip` toList bidders) $ \(bid, bidder) -> do
        withSender bidder $
          placeBid contract bid
      
      let sumBids = sum $ map (\bid -> Auction.priceParam bid `unsafeMulMutez` Auction.quantityParam bid) bids 
      
      contractBalance <- getBalance contract
      
      contractBalance @== sumBids

      -- Wait for the auction to end.
      advanceTime (sec $ fromIntegral $ testAuctionDuration `max` testExtendTime)

      returnBids contract (Auction.AuctionId 0, 6) --max bids
      auctionStorage <- getStorage' contract
      heapSize <- getHeapSize 0 auctionStorage
      if heapSize > 0
        then do
          returnOffers contract (Auction.AuctionId 0, 1000) --max offers in a bid
        else do
          pass

      withSender seller $ do
        resolveAuction contract

      unless (null winners) (payoutWinners contract (Auction.AuctionId 0, 100))

      postPayoutAuctionStorage <- getStorage' contract
      numWinningOffers <- getNumWinningOffers 0 postPayoutAuctionStorage
      winningPrice <- getWinningPrice 0 postPayoutAuctionStorage

      let maybeAuctionData = getAuction (Auction.AuctionId 0) postPayoutAuctionStorage
      auctionData <- case maybeAuctionData of
            Nothing -> failure $ "Auction not found for auction"
            Just auction -> pure $ auction
      let totalFeesSent = winningPrice `unsafeMulMutez` numWinningOffers
      let expectedProfitAmount = totalFeesSent 

      sellerBalanceAfter <- getBalance seller
      auctionContractBalanceAfter <- getBalance contract
      profitAddressBalanceAfter <- getBalance profitAddress

      sellerBalanceBefore @== sellerBalanceAfter
      profitAddressBalanceAfter @== profitAddressBalanceBefore `unsafeAddMutez` expectedProfitAmount
      auctionContractBalanceBefore @== auctionContractBalanceAfter

hprop_Failure_if_not_all_bids_returned :: Property
hprop_Failure_if_not_all_bids_returned =
  property $ do
    testData@TestData{testExtendTime, testAuctionDuration} <- forAll genTestData
    bids <- forAll $ genSomeBids testData

    clevelandProp $ do
      setup@Setup{seller, contract, fa2Contract, profitAddress} <- testSetup testData
      bidders <- mkBidders bids
      let numWinningBids = length $ winningBids sampleBondingCurve' (toList bids `zip` toList bidders)
      sellerBalanceBefore <- getBalance seller
      auctionContractBalanceBefore <- getBalance contract
      profitAddressBalanceBefore <- getBalance profitAddress

      withSender seller $ addSampleBondingCurve contract
      withSender seller $ configureAuction contract fa2Contract testData setup

      -- Wait for the auction to start.
      waitForAuctionToStart testData

      forM_ (toList bids `zip` toList bidders) $ \(bid, bidder) -> do
        withSender bidder $
          placeBid contract bid
      
      -- Wait for the auction to end.
      advanceTime (sec $ fromIntegral $ testAuctionDuration `max` testExtendTime)
      let correctNumBidsToReturn = length bids - numWinningBids

      if correctNumBidsToReturn <= 1
        then 
          pass 
        else
          returnBids contract (Auction.AuctionId 0, fromIntegral $ correctNumBidsToReturn - 1) --max bids
      

      if correctNumBidsToReturn >= 1
        then 
          withSender seller $ do
            resolveAuction contract
            & expectError errNotEmptied
      else pass

      

hprop_Bid_increase_works_as_expected :: Property
hprop_Bid_increase_works_as_expected  =
  property $ do
    testData@TestData{testExtendTime, testAuctionDuration} <- forAll genTestData
    bids <- forAll $ genUniqueBids testData

    clevelandProp $ do
      setup@Setup{seller, contract, fa2Contract, contract2, fa2Contract2, profitAddress} <- testSetup testData
      bidders <- mkBidders bids
      let winners = winningBids sampleBondingCurve' (toList bids `zip` toList bidders)

      withSender seller $ addSampleBondingCurve contract
      withSender seller $ addSampleBondingCurve contract2
      withSender seller $ configureAuction contract fa2Contract testData setup
      withSender seller $ configureAuction contract2 fa2Contract2 testData setup

      -- Wait for the auction to start.
      waitForAuctionToStart testData
      
      let increaseBid :: Auction.BidParam -> (Auction.BidParam, Mutez, Mutez) 
          increaseBid x = 
            let oldPrice = Auction.priceParam x
                newPrice = oldPrice `unsafeAddMutez` 50 in
              (x {Auction.quantityParam = Auction.quantityParam x + 100, 
                 Auction.priceParam = newPrice}, oldPrice, newPrice)

      let increaseHeadBid :: NonEmpty Auction.BidParam -> ((NonEmpty Auction.BidParam), Mutez, Mutez)
          increaseHeadBid (x :| a)  = ((newBid :| a), oldPrice, newPrice)
            where (newBid, oldPrice, newPrice) = increaseBid x

      let (bidList2, oldBidValue, newBidValue) = increaseHeadBid bids

      forM_ (toList bids `zip` toList bidList2 `zip` toList bidders) $ \((bid1, bid2), bidder) -> do
        withSender bidder $
          placeBid contract bid1
        withSender bidder $
          placeBid contract2 bid2
      
      st <- getStorage' contract

      let bidIncreaseKeyValues = st & Auction.bids & unBigMap & Map.filter (\bidValue -> Auction.price bidValue == oldBidValue) & Map.toList

      bidIncreaseKey <- case bidIncreaseKeyValues of
          [] -> failure $ "Empty list"
          [x] -> pure $ fst x
          (x : xs) -> failure $ "Multiple matching keys"

      let increaseBidParam = (head $ bidList2){Auction.isBidIncrease = Just bidIncreaseKey, Auction.priceParam = newBidValue}
      
      let originalFirstBid = head bids
      
      withSender (head bidders) $
        placeIncreaseBid contract increaseBidParam (Auction.priceParam originalFirstBid `unsafeMulMutez` Auction.quantityParam originalFirstBid)

      auctionStorage1Bids <- Auction.bidHeap <$> getStorage' contract
      let contract1Bids = Map.toList $ unBigMap auctionStorage1Bids
      
      auctionStorage2Bids <- Auction.bidHeap <$> getStorage' contract2
      let contract2Bids = Map.toList $ unBigMap auctionStorage2Bids
      (length contract1Bids) @== (length contract2Bids)
      
      --Now, testing pays out the same

      advanceTime (sec $ fromIntegral $ testAuctionDuration `max` testExtendTime)

      returnBids contract (Auction.AuctionId 0, 6) --max bids
      returnBids contract2 (Auction.AuctionId 0, 6) --max bids

      auctionStorage <- getStorage' contract
      auctionStorage2 <- getStorage' contract2

      heapSize <- getHeapSize 0 auctionStorage
      if heapSize > 0
        then do
          returnOffers contract (Auction.AuctionId 0, 1000) --max offers in a bid
        else do
          pass
      
      heapSize2 <- getHeapSize 0 auctionStorage2
      if heapSize2 > 0
        then do
          returnOffers contract2 (Auction.AuctionId 0, 1000) --max offers in a bid
        else do
          pass

      withSender seller $ do
        resolveAuction contract
        resolveAuction contract2

      unless (null winners) (payoutWinners contract (Auction.AuctionId 0, 100))
      unless (null winners) (payoutWinners contract2 (Auction.AuctionId 0, 100))

      postPayoutAuctionStorage1 <- getStorage' contract
      postPayoutAuctionStorage2 <- getStorage' contract2
      numWinningOffers1 <- getNumWinningOffers 0 postPayoutAuctionStorage1
      numWinningOffers2 <- getNumWinningOffers 0 postPayoutAuctionStorage2
      winningPrice1 <- getWinningPrice 0 postPayoutAuctionStorage1
      winningPrice2 <- getWinningPrice 0 postPayoutAuctionStorage2
      numWinningOffers1 @== numWinningOffers2
      winningPrice1 @== winningPrice2

      contract1Balance <- getBalance contract
      contract2Balance <- getBalance contract2
      contract1Balance @== contract2Balance

      fa2Storage1 <- getStorage' fa2Contract 
      fa2Storage2 <- getStorage' fa2Contract2
      let ledger1 = NftToken.ledger' $ Nft.assets' fa2Storage1
      let ledger2 = NftToken.ledger' $ Nft.assets' fa2Storage2
      ledger1 @== ledger2
      

hprop_Bid_and_return_commute_for_increasing_bids :: Property
hprop_Bid_and_return_commute_for_increasing_bids  =
  property $ do
    testData@TestData{testExtendTime, testAuctionDuration} <- forAll genTestData
    bids <- forAll $ genUniqueBids testData

    clevelandProp $ do
      setup@Setup{seller, contract, fa2Contract, contract2, fa2Contract2, profitAddress} <- testSetup testData
      bidders <- mkBidders bids
      let winners = winningBids sampleBondingCurve' (toList bids `zip` toList bidders)

      withSender seller $ addSampleBondingCurve contract
      withSender seller $ addSampleBondingCurve contract2
      withSender seller $ configureAuction contract fa2Contract testData setup
      withSender seller $ configureAuction contract2 fa2Contract2 testData setup

      -- Wait for the auction to start.
      waitForAuctionToStart testData

      --Returning all possible bids and offers for contract1 iteratively
      --Only returning possible bids/offers at end for contract2

      forM_ (toList bids `zip` toList bidders) $ \(bid, bidder) -> do
        withSender bidder $ do
          placeBid contract bid
          placeBid contract2 bid
        withSender seller $
          returnBids contract (Auction.AuctionId 0, 6) 
      
      withSender seller$ 
        returnBids contract2 (Auction.AuctionId 0, 6)
      
      auctionStorage1BidHeap <- Auction.bidHeap <$> getStorage' contract
      let contract1BidHeap = Map.toList $ unBigMap auctionStorage1BidHeap
      auctionStorage2BidHeap <- Auction.bidHeap <$> getStorage' contract2
      let contract2BidHeap = Map.toList $ unBigMap auctionStorage2BidHeap
      (length contract1BidHeap) @== (length contract2BidHeap)
      --Now, testing pays out the same

      advanceTime (sec $ fromIntegral $ testAuctionDuration `max` testExtendTime)

      auctionStorage <- getStorage' contract
      heapSize <- getHeapSize 0 auctionStorage
      if heapSize > 0
        then do
          returnOffers contract (Auction.AuctionId 0, 1000) --max offers in a bid
        else do
          pass

      auctionStorage2 <- getStorage' contract2
      heapSize2 <- getHeapSize 0 auctionStorage2
      if heapSize2 > 0
        then do
          returnOffers contract2 (Auction.AuctionId 0, 1000) --max offers in a bid
        else do
          pass

      withSender seller $ do
        resolveAuction contract
        resolveAuction contract2

      unless (null winners) (payoutWinners contract (Auction.AuctionId 0, 100))
      unless (null winners) (payoutWinners contract2 (Auction.AuctionId 0, 100))

      postPayoutAuctionStorage1 <- getStorage' contract
      postPayoutAuctionStorage2 <- getStorage' contract2
      numWinningOffers1 <- getNumWinningOffers 0 postPayoutAuctionStorage1
      numWinningOffers2 <- getNumWinningOffers 0 postPayoutAuctionStorage2
      winningPrice1 <- getWinningPrice 0 postPayoutAuctionStorage1
      winningPrice2 <- getWinningPrice 0 postPayoutAuctionStorage2
      numWinningOffers1 @== numWinningOffers2
      winningPrice1 @== winningPrice2


      contract1Balance <- getBalance contract
      contract2Balance <- getBalance contract2
      contract1Balance @== contract2Balance

      fa2Storage1 <- getStorage' fa2Contract 
      fa2Storage2 <- getStorage' fa2Contract2
      let ledger1 = NftToken.ledger' $ Nft.assets' fa2Storage1
      let ledger2 = NftToken.ledger' $ Nft.assets' fa2Storage2
      ledger1 @== ledger2


hprop_First_bid_is_valid_IFF_it_meets_price_floor :: Property
hprop_First_bid_is_valid_IFF_it_meets_price_floor =
  property $ do

    testData@TestData{testPriceFloor} <- forAll genTestData
    firstBid <- forAll $ genMutez' (Range.linear 0 (testPriceFloor * 2))

    clevelandProp $ do
      setup@Setup{seller, startTime, contract, fa2Contract} <- testSetup testData
      bidder <- newAddress "bidder"
      withSender seller $ addSampleBondingCurve contract
      withSender seller $ configureAuction contract fa2Contract testData setup
      waitForAuctionToStart testData
      let bid = Auction.BidParam 0 firstBid 1 Nothing 

      withSender bidder $
        if firstBid >= testPriceFloor
          then placeBid contract bid
          else placeBid contract bid
               & expectFailedWith
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
      setup@Setup{seller, contract, fa2Contract} <- testSetup testData'
      bidders <- mkBidders bids
      withSender seller $ addSampleBondingCurve contract
      withSender seller $ configureAuction contract fa2Contract testData' setup
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
              resolveAuction contract
                & expectError [mt|AUCTION_NOT_ENDED|]

              placeBids (rest, successfulBids + 1)

            else do
              withSender bidder $ do
                placeBid contract bid
                  & expectError [mt|NOT_IN_PROGRESS|]

              -- return bids, max 6 bids
              if successfulBids > 0
                then do
                  returnBids contract (Auction.AuctionId 0, 6) --max bids
                else do
                  pass

              auctionStorage <- getStorage' contract
              heapSize <- getHeapSize 0 auctionStorage

              if heapSize > 0
                then do
                  returnOffers contract (Auction.AuctionId 0, 1000) --max offers in a bid
                else do
                  pass

              -- `resolve` should succeed.
              resolveAuction contract


      placeBids (toList bids `zip` toList bidders `zip` toList bidWaitingTimes, 0)

hprop_Assets_are_transferred_to_winning_bidders :: Property
hprop_Assets_are_transferred_to_winning_bidders =
  property $ do
    testData@TestData{testExtendTime, testAuctionDuration} <- forAll genTestData
    bids <- forAll $ genSomeBids testData

    clevelandProp $ do
      setup@Setup{seller, contract, fa2Contract} <- testSetup testData
      bidders <- mkBidders bids
      withSender seller $ addSampleBondingCurve contract
      withSender seller $ configureAuction contract fa2Contract testData setup

      -- Wait for the auction to start.
      waitForAuctionToStart testData

      forM_ (toList bids `zip` toList bidders) $ \(bid, bidder) -> do
        withSender bidder $
          placeBid contract bid

      -- Wait for the auction to end.
      advanceTime (sec $ fromIntegral $ testAuctionDuration `max` testExtendTime)

      returnBids contract (Auction.AuctionId 0, 6) --max bids
      auctionStorage <- getStorage' contract
      heapSize <- getHeapSize 0 auctionStorage
      if heapSize > 0
        then do
          returnOffers contract (Auction.AuctionId 0, 1000) --max offers in a bid
        else do
          pass

      withSender seller $ do
        resolveAuction contract

      let winners = winningBids sampleBondingCurve' (toList bids `zip` toList bidders)

      unless (null winners) (payoutWinners contract (Auction.AuctionId 0, 100))


      -- All the tokens should have been transferred to the winners.
      let increment n = n + 1
      count <- newIORef 1
      forM_ winners \(winningBid, winner) -> do
        forM_ [1..(Auction.quantityParam winningBid)] \_ -> do
          tokenId <- readIORef count
          winnerBalance <- Nft.balanceOf fa2Contract (FA2I.TokenId tokenId) winner
          winnerBalance @== 1
          modifyIORef' count increment

-------------------------------------
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

genUniqueBids :: TestData -> Gen (NonEmpty Auction.BidParam)
genUniqueBids testData = do 
  len <- Gen.integral (Range.linear 0 5)
  firstBid <- genBid testData 0
  moreBids <- forM [1 .. len] (\bidIndex -> genBid testData{testPriceFloor = (toMutez 2e6) `unsafeMulMutez` bidIndex} 0) 
  pure $ firstBid :| moreBids

-- | Generate a valid bid, such that it is greater than the opening price.
genBid :: TestData -> Natural -> Gen Auction.BidParam
genBid testData auctionId = do
  let minBid = testPriceFloor testData
  let isBidIncrease = Nothing 
  bidPrice <- genMutez' (Range.linear minBid (minBid + 1_000_000))
  quantity <- Gen.integral (Range.linear 1 1000)
  pure $ Auction.BidParam auctionId bidPrice quantity isBidIncrease

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------
-- @== safe for off by 1 rounding errors 
-- (@~==) :: (HasCallStack, MonadNettest caps base m) => Mutez -> Mutez -> m()
-- (@~==) a b
--   | a == b = true
--   | a > b = a @== bPlusOne
--   | otherwise = b @== aPlusOne
--   where 
--     bPlusOne = b `unsafeAddMutez` (toMutez 1 :: Mutez)
--     aPlusOne = a `unsafeAddMutez` (toMutez 1 :: Mutez)

mkBidders :: (MonadNettest caps base m, TraversableWithIndex Int f) => f Auction.BidParam -> m (f Address)
mkBidders bids =
  ifor bids \i _ -> do
    addr <- newAddress (fromString $ "bidder-" <> show i)
    transferMoney addr 1_000_000_e6 --transfer sufficient tez to bidders
    pure addr

sampleBondingCurve :: '[Natural] L.:-> '[Mutez]
sampleBondingCurve = do
  L.push (toMutez 333333 :: Mutez) L.# L.mul

sampleBondingCurve' :: Natural -> Mutez
sampleBondingCurve' qty =
  (toMutez 333333 :: Mutez) `unsafeMulMutez` qty

data Setup = Setup
  { contract :: ContractHandler Auction.AuctionEntrypoints Auction.AuctionStorage
  , contract2 :: ContractHandler Auction.AuctionEntrypoints Auction.AuctionStorage
  , fa2Contract :: ContractHandler Nft.EntrypointsWithMint Nft.StorageWithTokenMetadata
  , fa2Contract2 :: ContractHandler Nft.EntrypointsWithMint Nft.StorageWithTokenMetadata
  , feeCollector :: Address
  , seller :: Address
  , startTime :: Timestamp
  , endTime :: Timestamp
  , profitAddress :: Address
  }

testSetup :: MonadNettest caps base m => TestData -> m Setup
testSetup testData = do
  feeCollector <- newAddress "fee-collector"
  profitAddress <- newAddress "profit-address"

  seller <- newAddress "seller"
  contract <- originateAuction seller
  contract2 <- originateAuction seller

  -- The FA2 contracts with the seller's assets, contact is the admin
  fa2Contract <- originateNftContract (toAddress contract)
  fa2Contract2 <- originateNftContract (toAddress contract2)

  now <- getNow
  let startTime = now `timestampPlusSeconds` testTimeToStart testData
  let endTime = startTime `timestampPlusSeconds` fromIntegral (testAuctionDuration testData)

  pure Setup {..}

waitForAuctionToStart :: (HasCallStack, MonadNettest caps base m) => TestData -> m ()
waitForAuctionToStart TestData{testTimeToStart} =
  advanceTime (sec $ fromIntegral testTimeToStart)

getAuction :: Auction.AuctionId -> Auction.AuctionStorage -> Maybe Auction.Auction
getAuction auctionId st = do
        st
          & Auction.auctions
          & unBigMap
          & Map.lookup auctionId

getHeapSize :: MonadNettest caps base m => Natural -> Auction.AuctionStorage -> m Natural
getHeapSize auctionId auctionStorage = do
  let heapSize = auctionStorage
                    & Auction.heapSizes
                    & unBigMap
                    & Map.lookup (Auction.AuctionId auctionId)
  case heapSize of
      Just hs -> pure hs
      Nothing -> pure 0

getNumWinningOffers :: MonadNettest caps base m => Natural -> Auction.AuctionStorage -> m Natural
getNumWinningOffers auctionId auctionStorage = do
  let auction = getAuction (Auction.AuctionId auctionId) auctionStorage
  case auction of
    Nothing -> failure $ "Auction not found for auction with Auction ID '" +| auctionId |+ "'"
    Just auction -> do
      let winnersOpt = Auction.numWinningOffers auction
      case winnersOpt of
          Just numWinningOffers -> pure numWinningOffers
          Nothing -> failure $ "Number of Winning Offers is not determined yet for auction with Auction ID '" +| auctionId |+ "'"

getWinningPrice :: MonadNettest caps base m => Natural -> Auction.AuctionStorage -> m Mutez
getWinningPrice auctionId auctionStorage = do
  let auction = getAuction (Auction.AuctionId auctionId) auctionStorage
  case auction of
    Nothing -> failure $ "Auction not found for auction with Auction ID '" +| auctionId |+ "'"
    Just auction -> do
      let winningPriceOpt = Auction.winningPrice auction
      case winningPriceOpt of
          Just winningPrice -> pure winningPrice
          Nothing -> failure $ "Winning price is not determined yet for auction with Auction ID '" +| auctionId |+ "'"



--maxAcceptedOffersAtPrice :: ('[Natural] L.:-> '[Mutez]) -> Mutez -> Natural -> Natural 
--maxAcceptedOffersAtPrice bondingCurve offerPrice totalOffers = do
--  let maybeMaxAcceptedOffers = find ( (>= offerPrice) . minPriceValidAtQ) [1 .. totalOffers]
--  fromMaybe totalOffers maybeMaxAcceptedOffers
--  where minPriceValidAtQ = \q -> L.exec q bondingCurve 

--Builds list in reverse order from highest to lowest bids as we would expect
winningBids :: (Natural -> Mutez) -> [(Auction.BidParam, Address)] -> [(Auction.BidParam, Address)]
winningBids bondingCurve bids = do
  let bidsSorted = sort bids
  let numOffers = foldl (\offers bid -> offers + Auction.quantityParam (fst bid)) 0 bidsSorted
  let
    getValidBidList ([], _) = []
    getValidBidList ((x:xs), numOffs) =
      let expectedMinimumPrice = bondingCurve $ numOffs
          bidParam = fst x
          bidder =  snd x
      in
      if Auction.priceParam bidParam < expectedMinimumPrice
        then
          if Auction.quantityParam bidParam == 1
            then getValidBidList (xs, numOffs - (Auction.quantityParam $ fst x))
          else
            getValidBidList ((bidParam{Auction.quantityParam = ((Auction.quantityParam $ fst x) - 1)}, bidder) : xs, numOffs - 1)
        else
          x : xs
  getValidBidList (bidsSorted, numOffers)
--  let maybeMaxAcceptedOffers = find ( (>= offerPrice) . minPriceValidAtQ) [1 .. totalOffers]
--  fromMaybe totalOffers maybeMaxAcceptedOffers
--  where minPriceValidAtQ = \q -> L.exec q bondingCurve 

----------------------------------------------------------------------------
-- Call entrypoints
----------------------------------------------------------------------------

configureAuction :: (HasCallStack, MonadNettest caps base m) => ContractHandler Auction.AuctionEntrypoints Auction.AuctionStorage -> ContractHandler Nft.EntrypointsWithMint Nft.StorageWithTokenMetadata -> TestData -> Setup -> m ()
configureAuction contract fa2Contract testData Setup{startTime, endTime, profitAddress}  = do

  call contract (Call @"Configure") Auction.ConfigureParam
    { priceFloor = testPriceFloor testData
    , roundTime = testRoundTime testData
    , extendTime = testExtendTime testData
    , fa2Address = toAddress fa2Contract
    , startTime = startTime
    , endTime = endTime
    , bondingCurve = 0
    , profitAddress = profitAddress
    , tokenInfo = mempty
    }

placeBid :: (HasCallStack, MonadNettest caps base m) => ContractHandler Auction.AuctionEntrypoints Auction.AuctionStorage -> Auction.BidParam -> m ()
placeBid contract bidParam =
    transfer TransferData
      { tdTo = contract
      , tdAmount = (Auction.priceParam bidParam) `unsafeMulMutez` (Auction.quantityParam bidParam)
      , tdEntrypoint = ep "bid"
      , tdParameter = bidParam
      }

placeIncreaseBid :: (HasCallStack, MonadNettest caps base m) => ContractHandler Auction.AuctionEntrypoints Auction.AuctionStorage -> Auction.BidParam -> Mutez -> m ()
placeIncreaseBid contract bidParam oldFee =
    transfer TransferData
      { tdTo = contract
      , tdAmount = ((Auction.priceParam bidParam) `unsafeMulMutez` (Auction.quantityParam bidParam)) `unsafeSubMutez` oldFee
      , tdEntrypoint = ep "bid"
      , tdParameter = bidParam
      }    

addSampleBondingCurve :: (HasCallStack, MonadNettest caps base m) => ContractHandler Auction.AuctionEntrypoints Auction.AuctionStorage -> m ()
addSampleBondingCurve = addBondingCurve sampleBondingCurve 

addBondingCurve :: (HasCallStack, MonadNettest caps base m) => ('[Natural] L.:-> '[Mutez])  -> ContractHandler Auction.AuctionEntrypoints Auction.AuctionStorage -> m ()
addBondingCurve bc contract = do
  call contract (Call @"Add_bonding_curve") bc

resolveAuction :: (HasCallStack, MonadNettest caps base m) => ContractHandler Auction.AuctionEntrypoints Auction.AuctionStorage -> m ()
resolveAuction contract =
  call contract (Call @"Resolve") (Auction.AuctionId 0)

cancelAuction :: (HasCallStack, MonadNettest caps base m) => ContractHandler Auction.AuctionEntrypoints Auction.AuctionStorage -> m ()
cancelAuction contract =
  call contract (Call @"Cancel") (Auction.AuctionId 0)

returnBids :: (HasCallStack, MonadNettest caps base m) => ContractHandler Auction.AuctionEntrypoints Auction.AuctionStorage -> (Auction.AuctionId, Natural) -> m ()
returnBids contract =
  call contract (Call @"Return_old_bids")

returnOffers:: (HasCallStack, MonadNettest caps base m) => ContractHandler Auction.AuctionEntrypoints Auction.AuctionStorage -> (Auction.AuctionId, Natural) -> m ()
returnOffers contract =
  call contract (Call @"Return_old_offers")

payoutWinners :: (HasCallStack, MonadNettest caps base m) => ContractHandler Auction.AuctionEntrypoints Auction.AuctionStorage -> (Auction.AuctionId, Natural) -> m ()
payoutWinners contract =
  call contract (Call @"Payout_winners")

errNotEmptied :: MText
errNotEmptied = [mt|NOT_ALL_INVALID_BIDS_AND_OFFERS_RETURNED|]