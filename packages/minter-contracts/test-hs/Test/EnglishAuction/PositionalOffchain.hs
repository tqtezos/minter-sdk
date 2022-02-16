module Test.EnglishAuction.PositionalOffchain where

import Cleveland.Util (sec)
import qualified Data.List as List
import qualified Data.Map as Map
import Hedgehog (Gen, Property, forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Indigo.Contracts.FA2Sample as FA2
import Lorentz hiding (amount, contract, now)
import qualified Lorentz.Contracts.NoAllowlist as NoAllowlist
import Lorentz.Contracts.Spec.FA2Interface (TokenId(..))
import Morley.Nettest
import Test.Util 
import Tezos.Core (timestampPlusSeconds)
import qualified Lorentz.Contracts.EnglishAuction.ConsolationOffchain as ConsolationOffchain
import Test.EnglishAuction.Util
import qualified Lorentz.Contracts.EnglishAuction.Common as Common
import qualified Lorentz.Contracts.EnglishAuction.Consolation as Consolation
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2I
import Test.EnglishAuction.ConsolationOffchain hiding (Setup, genTestData, testSetup, configureAuction)

hprop_Send_consolation_iteratively_correctly_distributes_tokens :: Property
hprop_Send_consolation_iteratively_correctly_distributes_tokens =
  property $ do
    testData@TestData{testExtendTime, testAuctionDuration, testTokenBatches, testMaxConsolationWinners} <- forAll genTestData
    bids <- forAll $ genSomeBids testData

    clevelandProp $ do
      setup@Setup{seller, contract, fa2Contracts, positionalFa2Contract} <- testSetup testData
      bidders <- mkBidders bids
      let numBids = fromIntegral $ length (toList bids) 
      withSender seller $ configureAuction testData setup

      -- Wait for the auction to start.
      waitForAuctionToStart testData

      forM_ (toList bids `zip` toList bidders) \(bid, bidder) -> do
        withSender bidder $
          placeBid contract bid

      -- Wait for the auction to end.
      advanceTime (sec $ fromIntegral $ testAuctionDuration `max` testExtendTime)

      let consolationReceivers = genConsolationWinners numBids testMaxConsolationWinners

      let numConsolationReceivers = length consolationReceivers
      
      when (numBids > 1 && testMaxConsolationWinners > 0) 
        (forM_ [1 .. numConsolationReceivers] \_ -> do
          withSender seller $ 
            sendConsolation 1 contract
        )

      withSender seller $
        resolveAuction contract

      let winner = last bidders

      -- All the tokens should have been transferred to the winner.
      forM_ (testTokenBatches `zip` fa2Contracts) \(tokenBatch, fa2Contract) -> do
        let expectedBalances =
              tokenBatch
                <&> (\Common.FA2Token{tokenId, amount} -> (tokenId, amount))
                & Map.fromListWith (+)
        forM_ (Map.toList expectedBalances) \(tokenId, expectedBalance) -> do
          sellerBalance <- balanceOf fa2Contract tokenId seller
          sellerBalance @== 0
          contractBalance <- balanceOf fa2Contract tokenId contract
          contractBalance @== 0
          winnerBalance <- balanceOf fa2Contract tokenId winner
          winnerBalance @== expectedBalance

      -- Tests positional tokens are received    
      forM_ (toList bidders `zip` [1 .. numBids - 1]) \(bidder, bidIndex) -> do 
          consolationBalance <- 
            balanceOf positionalFa2Contract (FA2I.TokenId $ ((numBids - 1) - bidIndex)) bidder
          if bidIndex `elem` consolationReceivers  
              then consolationBalance @== 1 
          else consolationBalance @== 0 

hprop_Assets_are_transferred_to_highest_bidder_after_positional_tokens_sent :: Property
hprop_Assets_are_transferred_to_highest_bidder_after_positional_tokens_sent =
  property $ do
    testData@TestData{testExtendTime, testAuctionDuration, testTokenBatches, testMaxConsolationWinners} <- forAll genTestData
    bids <- forAll $ genSomeBids testData

    clevelandProp $ do
      setup@Setup{seller, contract, fa2Contracts, positionalFa2Contract} <- testSetup testData
      bidders <- mkBidders bids
      let numBids = fromIntegral $ length (toList bids) 
      withSender seller $ configureAuction testData setup

      -- Wait for the auction to start.
      waitForAuctionToStart testData

      forM_ (toList bids `zip` toList bidders) \(bid, bidder) -> do
        withSender bidder $
          placeBid contract bid

      -- Wait for the auction to end.
      advanceTime (sec $ fromIntegral $ testAuctionDuration `max` testExtendTime)
      
      when (numBids > 1 && testMaxConsolationWinners > 0) (withSender seller $ 
          sendConsolation numBids contract)

      withSender seller $
        resolveAuction contract

      let winner = last bidders

      -- All the tokens should have been transferred to the winner.
      forM_ (testTokenBatches `zip` fa2Contracts) \(tokenBatch, fa2Contract) -> do
        let expectedBalances =
              tokenBatch
                <&> (\Common.FA2Token{tokenId, amount} -> (tokenId, amount))
                & Map.fromListWith (+)
        forM_ (Map.toList expectedBalances) \(tokenId, expectedBalance) -> do
          sellerBalance <- balanceOf fa2Contract tokenId seller
          sellerBalance @== 0
          contractBalance <- balanceOf fa2Contract tokenId contract
          contractBalance @== 0
          winnerBalance <- balanceOf fa2Contract tokenId winner
          winnerBalance @== expectedBalance
     
      let consolationReceivers = genConsolationWinners numBids testMaxConsolationWinners

      -- Tests consolation tokens are received    
      forM_ (toList bidders `zip` [1 .. numBids - 1]) \(bidder, bidIndex) -> do 
          consolationBalance <- balanceOf positionalFa2Contract (FA2I.TokenId ((numBids - 1) - bidIndex)) bidder
          if bidIndex `elem` consolationReceivers  
              then consolationBalance @== 1 
          else consolationBalance @== 0 

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

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

  testMaxConsolationWinners <- Gen.integral (Range.linear 0 6) -- Max 6 set for positional to limit number tokens needed to define in asset contract

  testAuctionDuration <- Gen.integral (Range.linear 1 100)
  testMaxAuctionTime <- Gen.integral (Range.linear testAuctionDuration (testAuctionDuration * 2))

  testTokenBatches <-
    Gen.list (Range.linear 0 10) $
        Gen.list (Range.linear 0 10) $
        Common.FA2Token
          <$> (TokenId <$> Gen.integral (Range.linear 0 10))
          <*> Gen.integral (Range.linear 0 10)

  pure $ TestData {..}

data Setup = Setup
  { contract :: TAddress (ConsolationOffchain.AuctionEntrypoints NoAllowlist.Entrypoints)
  , fa2Contracts :: [TAddress FA2.FA2SampleParameter]
  , positionalFa2Contract :: TAddress FA2.FA2SampleParameter 
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
    originateAuctionTezOffchainPositional seller

  -- The FA2 contracts with the seller's assets
  fa2Contracts <- forM (testTokenBatches testData) \tokenBatch -> do
    let tokenIds = List.nub $ Common.tokenId <$> tokenBatch
    let ledger =
          Map.fromListWith (+) $
            tokenBatch <&> \Common.FA2Token{tokenId, amount} -> ((seller, tokenId), amount)

    originateSimple "asset-fa2"
      FA2.Storage
      { sLedger = BigMap ledger
      , sOperators = BigMap $ Map.fromList [((seller, toAddress contract), ())]
      , sTokenMetadata = mempty
      }
      (FA2.fa2Contract def { FA2.cAllowedTokenIds = tokenIds })

  -- The FA2 contract with the consolation token
  positionalFa2Contract <- do
    let ledger =
          Map.fromList $
             [((seller, FA2I.TokenId 0), 1000), 
              ((seller, FA2I.TokenId 1), 1000),
              ((seller, FA2I.TokenId 2), 1000),
              ((seller, FA2I.TokenId 3), 1000),
              ((seller, FA2I.TokenId 4), 1000),
              ((seller, FA2I.TokenId 5), 1000)]

    originateSimple "consolation-fa2"
      FA2.Storage
      { sLedger = BigMap ledger
      , sOperators = BigMap $ Map.fromList [((seller, toAddress contract), ())]
      , sTokenMetadata = mempty
      }
      (FA2.fa2Contract def { FA2.cAllowedTokenIds = [FA2I.TokenId 0, 
                                                     FA2I.TokenId 1, 
                                                     FA2I.TokenId 2,
                                                     FA2I.TokenId 3,
                                                     FA2I.TokenId 4,
                                                     FA2I.TokenId 5] })

  now <- getNow
  let startTime = now `timestampPlusSeconds` testTimeToStart testData
  let endTime = startTime `timestampPlusSeconds` fromIntegral (testAuctionDuration testData)

  pure Setup {..}

configureAuction :: (HasCallStack, MonadNettest caps base m) => TestData -> Setup -> m ()
configureAuction testData Setup{fa2Contracts, contract, startTime, endTime, positionalFa2Contract}  = do
  let assets =
        (testTokenBatches testData `zip` fa2Contracts) <&> \(tokenBatch, fa2Contract) ->
          Common.Tokens
            { fa2Address = toAddress fa2Contract
            , fa2Batch = tokenBatch
            }

  call contract (Call @"Configure") Consolation.ConfigureParam
    { openingPrice = testOpeningPrice testData
    , minRaisePercent = testMinRaisePercent testData
    , minRaise = testMinRaise testData
    , roundTime = testRoundTime testData
    , extendTime = testExtendTime testData
    , asset = assets
    , startTime = startTime
    , endTime = endTime
    , consolationToken = Consolation.GlobalTokenId (toAddress positionalFa2Contract) (FA2I.TokenId Consolation.consolationTokenId)
    , maxConsolationWinners = testMaxConsolationWinners testData
    }
