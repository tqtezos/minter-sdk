-- | Tests on the swaps contract.
module Test.Swaps.Basic where

import Prelude hiding (swap)

import GHC.Integer (negateInteger)

import Test.Tasty (TestTree, testGroup)

import Morley.Nettest
import Morley.Nettest.Tasty (nettestScenarioCaps)
import Tezos.Address (unsafeParseAddress)

import Hedgehog (Gen, Property, forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Lorentz.Contracts.Swaps.Basic
import Lorentz.Test.Consumer
import Lorentz.Value
import Test.Swaps.Util
import Test.Util

data TestData = TestData
  { numOffers :: Natural
  , token1Offer :: Natural
  , token2Offer :: Natural
  , token1Request :: Natural
  , token2Request :: Natural
  }
  deriving stock (Show)

genTestData :: Gen TestData
genTestData = do
  let genNat = Gen.integral (Range.constant 1 20)
  numOffers <- genNat
  token1Offer <- genNat
  token2Offer <- genNat 
  token1Request <- genNat
  token2Request <- genNat
  pure $ TestData
    { numOffers = numOffers
    , token1Offer = token1Offer
    , token2Offer = token2Offer
    , token1Request = token1Request
    , token2Request = token2Request }

hprop_Correct_num_tokens_transferred_to_contract_on_start :: Property
hprop_Correct_num_tokens_transferred_to_contract_on_start = 
  property $ do
   TestData{numOffers, token1Offer, token2Offer} <- forAll genTestData 
   clevelandProp  $ do
     setup <- doFA2Setup
     let alice ::< SNil = sAddresses setup
     let tokenId1 ::< tokenId2 ::< SNil = sTokens setup
     swap <- originateSwap
     fa2 <- originateFA2 "fa2" setup [swap]
     assertingBalanceDeltas fa2
       [ (alice, tokenId1) -: negateInteger (fromIntegral $ token1Offer * numOffers)
       , (alice, tokenId2) -: negateInteger (fromIntegral $ token2Offer * numOffers)
       ] $ do
         withSender alice $
           call swap (Call @"Start") $ mkNOffers numOffers SwapOffer
             { assetsOffered = [mkFA2Assets fa2 [(tokenId1, token1Offer), (tokenId2, token2Offer)]]
             , assetsRequested = [mkFA2Assets fa2 [(tokenId2, 1)]]
             }

hprop_Contract_balance_goes_to_zero_when_sale_concludes :: Property
hprop_Contract_balance_goes_to_zero_when_sale_concludes = 
  property $ do
   TestData{numOffers, token1Offer, token2Offer, token1Request, token2Request} <- forAll genTestData 
   clevelandProp  $ do
     setup <- doFA2Setup
     let alice ::< bob ::< SNil = sAddresses setup
     let tokenId1 ::< tokenId2 ::< SNil = sTokens setup
     swap <- originateSwap
     let swapAddress = toAddress swap
     fa2 <- originateFA2 "fa2" setup [swap]
     assertingBalanceDeltas fa2
       [ (swapAddress, tokenId1) -: 0
       , (swapAddress, tokenId2) -: 0
       ] $ do
         withSender alice $
           call swap (Call @"Start") $ mkNOffers numOffers SwapOffer
             { assetsOffered = [mkFA2Assets fa2 [(tokenId1, token1Offer), (tokenId2, token2Offer)]]
             , assetsRequested = [mkFA2Assets fa2 [(tokenId1, token1Request), (tokenId2, token2Request)]]
             }
         withSender bob $
            replicateM_ (fromIntegral numOffers) $ do
              call swap (Call @"Accept") (SwapId 0)

test_Swap :: TestTree
test_Swap = testGroup "Basic swap functionality"
  [ simpleHappyPaths
  , statusChecks
  , swapIdChecks
  , authorizationChecks
  , invalidFA2sChecks
  , complexCases
  ]

simpleHappyPaths :: TestTree
simpleHappyPaths = testGroup "Simple happy paths"
  [ nettestScenarioCaps "Simple accepted swap" $ do
      setup <- doFA2Setup
      let alice ::< bob ::< SNil = sAddresses setup
      let tokenId1 ::< tokenId2 ::< SNil = sTokens setup
      swap <- originateSwap
      fa2 <- originateFA2 "fa2" setup [swap]

      assertingBalanceDeltas fa2
        [ (alice, tokenId1) -: -10
        , (alice, tokenId2) -: 5
        , (bob, tokenId1) -: 10
        , (bob, tokenId2) -: -5
        ] $ do
          withSender alice $
            call swap (Call @"Start") $ mkSingleOffer SwapOffer
              { assetsOffered = [mkFA2Assets fa2 [(tokenId1, 10)]]
              , assetsRequested = [mkFA2Assets fa2 [(tokenId2, 5)]]
              }
          withSender bob $
            call swap (Call @"Accept") (SwapId 0)

  , nettestScenarioCaps "Simple cancelled swap" $ do
      setup <- doFA2Setup
      let alice ::< SNil = sAddresses setup
      let tokenId1 ::< tokenId2 ::< SNil = sTokens setup
      swap <- originateSwap
      fa2 <- originateFA2 "fa2" setup [swap]

      assertingBalanceDeltas fa2
        [ (alice, tokenId1) -: 0
        , (alice, tokenId2) -: 0
        ] $ do
          withSender alice $
            call swap (Call @"Start") $ mkSingleOffer SwapOffer
              { assetsOffered = [mkFA2Assets fa2 [(tokenId1, 10)]]
              , assetsRequested = [mkFA2Assets fa2 [(tokenId2, 5)]]
              }
          withSender alice $
            call swap (Call @"Cancel") (SwapId 0)
  ]

statusChecks :: TestTree
statusChecks = testGroup "Statuses"
  [ nettestScenarioCaps "Operations with accepted swap fail" $ do
      swap <- originateSwap

      call swap (Call @"Start") $ mkSingleOffer $ SwapOffer [] []
      call swap (Call @"Accept") (SwapId 0)

      call swap (Call @"Accept") (SwapId 0)
        & expectError swap errSwapFinished

      call swap (Call @"Cancel") (SwapId 0)
        & expectError swap errSwapFinished

  , nettestScenarioCaps "Operations with cancelled swap fail" $ do
      swap <- originateSwap

      call swap (Call @"Start") $ mkSingleOffer $ SwapOffer [] []
      call swap (Call @"Cancel") (SwapId 0)

      call swap (Call @"Accept") (SwapId 0)
        & expectError swap errSwapCancelled

      call swap (Call @"Cancel") (SwapId 0)
        & expectError swap errSwapCancelled
  ]

remainingOffersChecks :: TestTree
remainingOffersChecks = testGroup "Statuses"
  [ nettestScenarioCaps "N+1th acceptance of N offers will fail" $ do
      swap <- originateSwap

      call swap (Call @"Start") $ mkNOffers 2 $ SwapOffer [] []
      call swap (Call @"Accept") (SwapId 0)
      call swap (Call @"Accept") (SwapId 0)

      call swap (Call @"Accept") (SwapId 0)
        & expectError swap errSwapFinished

      call swap (Call @"Cancel") (SwapId 0)
        & expectError swap errSwapFinished
  ]

swapIdChecks :: TestTree
swapIdChecks = testGroup "SwapIds"
  [ nettestScenarioCaps "Swap ids are properly assigned and can be worked with" $ do
      setup <- doFA2Setup
      let alice ::< bob ::< SNil = sAddresses setup
      let tokenId1 ::< tokenId2 ::< tokenId3 ::< SNil = sTokens setup
      swap <- originateSwap
      fa2 <- originateFA2 "fa2" setup [swap]

      withSender alice $
        for_ [tokenId1, tokenId2, tokenId3] $ \tokenId ->
          call swap (Call @"Start") $ mkSingleOffer SwapOffer
            { assetsOffered = []
            , assetsRequested = [mkFA2Assets fa2 [(tokenId, 1)]]
            }

      assertingBalanceDeltas fa2
        [ (bob, tokenId1) -: -1
        , (bob, tokenId2) -: -0
        , (bob, tokenId3) -: -1
        ] $ do
          withSender bob $ do
            call swap (Call @"Accept") (SwapId 0)
            call swap (Call @"Accept") (SwapId 2)

  , nettestScenarioCaps "Accessing non-existing swap fails respectively" $ do
      swap <- originateSwap

      call swap (Call @"Accept") (SwapId 0)
        & expectError swap errSwapNotExist
      call swap (Call @"Cancel") (SwapId 0)
        & expectError swap errSwapNotExist

      call swap (Call @"Start") $ mkSingleOffer $ SwapOffer [] []

      call swap (Call @"Accept") (SwapId 1)
        & expectError swap errSwapNotExist
      call swap (Call @"Cancel") (SwapId 1)
        & expectError swap errSwapNotExist

      call swap (Call @"Accept") (SwapId 0)
  ]

authorizationChecks :: TestTree
authorizationChecks = testGroup "Authorization checks"
  [ nettestScenarioCaps "Swap can be cancelled by seller only" $ do
      setup <- doFA2Setup
      let alice ::< bob ::< SNil = sAddresses setup
      let !SNil = sTokens setup
      swap <- originateSwap

      withSender alice $
        call swap (Call @"Start") $ mkSingleOffer $ SwapOffer [] []

      call swap (Call @"Cancel") (SwapId 0)
        & expectError swap errNotSwapSeller

      withSender bob $
        call swap (Call @"Cancel") (SwapId 0)
        & expectError swap errNotSwapSeller
  ]

invalidFA2sChecks :: TestTree
invalidFA2sChecks = testGroup "Invalid FA2s"
  [ nettestScenarioCaps "Swap can be cancelled by seller only" $ do
      setup <- doFA2Setup
      let alice ::< SNil = sAddresses setup
      let tokenId1 ::< SNil = sTokens setup

      fakeFa2 <-
        TAddress . unTAddress <$>
        originateSimple "fake-fa2" ([] :: [Integer]) contractConsumer
      let nonExistingFa2 = TAddress $ unsafeParseAddress "tz1b7p3PPBd3vxmMHZkvtC61C7ttYE6g683F"
      let pseudoFa2s = [("fake FA2", fakeFa2), ("non existing FA2", nonExistingFa2)]

      for_ pseudoFa2s $ \(desc, fa2) -> do
        comment $ "Trying " <> desc
        swap <- originateSwap

        comment "Checking offered FA2"
        withSender alice $
          call swap (Call @"Start") (mkSingleOffer SwapOffer
            { assetsOffered = [mkFA2Assets fa2 [(tokenId1, 1)]]
            , assetsRequested = []
            })
            & expectError swap errSwapOfferedFA2Invalid

        comment "Checking requested FA2"
        withSender alice $ do
          call swap (Call @"Start") $ mkSingleOffer SwapOffer
            { assetsOffered = []
            , assetsRequested = [mkFA2Assets fa2 [(tokenId1, 1)]]
            }

          call swap (Call @"Accept") (SwapId 0)
            & expectError swap errSwapRequestedFA2Invalid
  ]

complexCases :: TestTree
complexCases = testGroup "Complex cases"
  [ nettestScenarioCaps "Multiple FA2s" $ do
      setup <- doFA2Setup
      let alice ::< bob ::< SNil = sAddresses setup
      let tokenId1 ::< tokenId2 ::< tokenId3 ::< SNil = sTokens setup
      swap <- originateSwap
      fa2_1 <- originateFA2 "fa2-1" setup [swap]
      fa2_2 <- originateFA2 "fa2-2" setup [swap]

      assertingBalanceDeltas fa2_1
        [ (alice, tokenId1) -: -100
        , (alice, tokenId2) -: -50
        , (alice, tokenId3) -: 1
        , (bob, tokenId1) -: 100
        , (bob, tokenId2) -: 50
        , (bob, tokenId3) -: -1
        ] $
        assertingBalanceDeltas fa2_2
        [ (alice, tokenId1) -: -1000
        , (bob, tokenId1) -: 1000
        ] $ do
          withSender alice $
            call swap (Call @"Start") $ mkSingleOffer SwapOffer
              { assetsOffered =
                  [ mkFA2Assets fa2_1
                    [ (tokenId1, 100)
                    , (tokenId2, 50)
                    ]
                  , mkFA2Assets fa2_2
                    [ (tokenId1, 1000)
                    ]
                  ]
              , assetsRequested =
                  [ mkFA2Assets fa2_1
                    [ (tokenId3, 1)
                    ]
                  ]
              }
          withSender bob $
            call swap (Call @"Accept") (SwapId 0)

  ]
