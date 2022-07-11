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
import Tezos.Core (timestampPlusSeconds, unsafeMulMutez, mulMutez)
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
      let bid = Auction.BidParam 0 firstBid 1

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
      withSender seller $ configureAuction testData setup

      -- Wait for the auction to start.
      waitForAuctionToStart testData

      forM_ (toList bids `zip` toList bidders) \(bid, bidder) -> do
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


      -- All the tokens should have been transferred to the winner.
      let increment n = n + 1
      count <- newIORef 0
      forM_ winners \(winningBid, winner) -> do
        forM_ [1..(Auction.quantityParam winningBid)] \_ -> do
          tokenId <- readIORef count
          winnerBalance <- Nft.balanceOf fa2Contract (FA2I.TokenId tokenId) winner
          winnerBalance @== 1
          modifyIORef' count increment
      
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
  quantity <- Gen.integral (Range.linear 1 1000)
  pure $ Auction.BidParam auctionId bidPrice quantity

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

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

sampleIntegralCurve :: '[Natural] L.:-> '[Mutez]
sampleIntegralCurve = do
  L.dup L.# L.mul L.# L.push (toMutez 166666 :: Mutez) L.# L.mul 

data Setup = Setup
  { contract :: ContractHandler Auction.AuctionEntrypoints Auction.AuctionStorage
  , fa2Contract :: ContractHandler Nft.EntrypointsWithMint Nft.StorageWithTokenMetadata
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

  -- The FA2 contracts with the seller's assets, contact is the admin
  fa2Contract <- originateNftContract (toAddress contract)

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
  let heapSize = auctionStorage 
                    & Auction.heapSizes 
                    & unBigMap 
                    & Map.lookup (Auction.AuctionId auctionId)
  case heapSize of
      Just hs -> pure hs
      Nothing -> pure 0

--maxAcceptedOffersAtPrice :: ('[Natural] L.:-> '[Mutez]) -> Mutez -> Natural -> Natural 
--maxAcceptedOffersAtPrice bondingCurve offerPrice totalOffers = do
--  let maybeMaxAcceptedOffers = find ( (>= offerPrice) . minPriceValidAtQ) [1 .. totalOffers]
--  fromMaybe totalOffers maybeMaxAcceptedOffers
--  where minPriceValidAtQ = \q -> L.exec q bondingCurve 

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
            then getValidBidList (xs, numOffs)
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

placeBid :: (HasCallStack, MonadNettest caps base m) => ContractHandler Auction.AuctionEntrypoints Auction.AuctionStorage -> Auction.BidParam -> m ()
placeBid contract bidParam =
  transfer TransferData
    { tdTo = contract
    , tdAmount = (Auction.priceParam bidParam) `unsafeMulMutez` (Auction.quantityParam bidParam) 
    , tdEntrypoint = ep "bid"
    , tdParameter = bidParam
    }

addSampleBondingCurve :: (HasCallStack, MonadNettest caps base m) => ContractHandler Auction.AuctionEntrypoints Auction.AuctionStorage -> m ()
addSampleBondingCurve = addBondingCurve sampleBondingCurve sampleIntegralCurve

addBondingCurve :: (HasCallStack, MonadNettest caps base m) => ('[Natural] L.:-> '[Mutez])  -> ('[Natural] L.:-> '[Mutez])-> ContractHandler Auction.AuctionEntrypoints Auction.AuctionStorage -> m ()
addBondingCurve bc integral contract = do
  call contract (Call @"Add_bonding_curve") (bc, integral) 

resolveAuction :: (HasCallStack, MonadNettest caps base m) => ContractHandler Auction.AuctionEntrypoints Auction.AuctionStorage -> m ()
resolveAuction contract =
  call contract (Call @"Resolve") (Auction.AuctionId 0)

returnBids :: (HasCallStack, MonadNettest caps base m) => ContractHandler Auction.AuctionEntrypoints Auction.AuctionStorage -> (Auction.AuctionId, Natural) -> m ()
returnBids contract =  
  call contract (Call @"Return_old_bids")

returnOffers:: (HasCallStack, MonadNettest caps base m) => ContractHandler Auction.AuctionEntrypoints Auction.AuctionStorage -> (Auction.AuctionId, Natural) -> m ()
returnOffers contract =  
  call contract (Call @"Return_old_offers")

payoutWinners :: (HasCallStack, MonadNettest caps base m) => ContractHandler Auction.AuctionEntrypoints Auction.AuctionStorage -> (Auction.AuctionId, Natural) -> m ()
payoutWinners contract = 
  call contract (Call @"Payout_winners")