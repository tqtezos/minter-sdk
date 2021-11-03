-- | Tests on the swaps contract.
module Test.Swaps.Burn where

import Prelude hiding (swap)

import GHC.Exts (fromList)
import GHC.Integer (negateInteger)

import Hedgehog (Gen, Property, forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Tasty (TestTree, testGroup)

import Morley.Nettest
import Morley.Nettest.Tasty (nettestScenarioCaps)

import Lorentz.Contracts.Swaps.Basic
import Lorentz.Contracts.Swaps.Burn
import Lorentz.Contracts.Swaps.Allowlisted

import Test.Allowlisted
import Tezos.Address (unsafeParseAddress)
import Test.NonPausableSimpleAdmin

import Lorentz.Test.Consumer
import Lorentz.Value

import Test.Swaps.Util
import Test.Util

test_BurnSwap :: TestTree
test_BurnSwap = testGroup "Basic swap functionality"
  [ statusChecks
  , swapIdChecks
  , authorizationChecks
  , invalidFA2sChecks
  , complexCases
  ]

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

hprop_Correct_final_balances_on_acceptance :: Property
hprop_Correct_final_balances_on_acceptance = 
  property $ do
   TestData{numOffers, token1Offer, token2Offer, token1Request, token2Request} <- forAll genTestData 
   clevelandProp  $ do
      setup <- doFA2Setup
      let alice ::< bob ::< SNil = sAddresses setup
      let tokenId1 ::< tokenId2 ::< SNil = sTokens setup
      (swap, admin) <- originateAllowlistedBurnSwapWithAdmin
      fa2 <- originateFA2 "fa2" setup [swap]

      withSender admin $
        call swap (Call @"Update_allowed") (mkAllowlistSimpleParam [fa2])
 
      assertingBalanceDeltas fa2
        [ (alice, tokenId1) -: negateInteger (fromIntegral $ token1Offer * numOffers)
        , (alice, tokenId2) -: negateInteger (fromIntegral $ token2Offer * numOffers)
        , (nullAddress, tokenId1) -: (fromIntegral $ token1Request)
        , (nullAddress, tokenId2) -: (fromIntegral $ token2Request)
        , (bob, tokenId1) -: fromIntegral token1Offer - fromIntegral token1Request
        , (bob, tokenId2) -: fromIntegral token2Offer - fromIntegral token2Request
        ] $ do
          withSender alice $
            call swap (Call @"Start") $ mkNOffers numOffers SwapOffer
              { assetsOffered = [mkFA2Assets fa2 [(tokenId1, token1Offer), (tokenId2, token2Offer)]]
              , assetsRequested = [mkFA2Assets fa2 [(tokenId1, token1Request), (tokenId2, token2Request)]]
              }
          withSender bob $
            call swap (Call @"Accept") (SwapId 0)

hprop_Correct_final_balances_on_cancel :: Property
hprop_Correct_final_balances_on_cancel = 
  property $ do
   TestData{numOffers, token1Offer, token2Offer, token1Request, token2Request} <- forAll genTestData 
   clevelandProp  $ do
      setup <- doFA2Setup
      let alice ::< bob ::< SNil = sAddresses setup
      let tokenId1 ::< tokenId2 ::< SNil = sTokens setup
      (swap, admin) <- originateAllowlistedBurnSwapWithAdmin
      fa2 <- originateFA2 "fa2" setup [swap]

      withSender admin $
        call swap (Call @"Update_allowed") (mkAllowlistSimpleParam [fa2])

      assertingBalanceDeltas fa2
        [ (alice, tokenId1) -: 0
        , (alice, tokenId2) -: 0
        , (bob, tokenId1) -: 0
        , (bob, tokenId2) -: 0
        , (nullAddress, tokenId1) -: 0
        , (nullAddress, tokenId2) -: 0
        ] $ do
          withSender alice $
            call swap (Call @"Start") $ mkNOffers numOffers SwapOffer
              { assetsOffered = [mkFA2Assets fa2 [(tokenId1, token1Offer), (tokenId2, token2Offer)]]
              , assetsRequested = [mkFA2Assets fa2 [(tokenId1, token1Request), (tokenId2, token2Request)]]
              }
          withSender alice $
            call swap (Call @"Cancel") (SwapId 0)

hprop_Correct_num_tokens_transferred_to_contract_on_start :: Property
hprop_Correct_num_tokens_transferred_to_contract_on_start = 
  property $ do
   TestData{numOffers, token1Offer, token2Offer} <- forAll genTestData 
   clevelandProp  $ do
     setup <- doFA2Setup
     let alice ::< SNil = sAddresses setup
     let tokenId1 ::< tokenId2 ::< SNil = sTokens setup
     (swap, admin) <- originateAllowlistedBurnSwapWithAdmin
     fa2 <- originateFA2 "fa2" setup [swap]
     withSender admin $
        call swap (Call @"Update_allowed") (mkAllowlistSimpleParam [fa2])
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
     (swap, admin) <- originateAllowlistedBurnSwapWithAdmin
     let swapAddress = toAddress swap
     fa2 <- originateFA2 "fa2" setup [swap]
     withSender admin $
        call swap (Call @"Update_allowed") (mkAllowlistSimpleParam [fa2])
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

statusChecks :: TestTree
statusChecks = testGroup "Statuses"
  [ nettestScenarioCaps "Operations with accepted swap fail" $ do
      (swap, _) <- originateAllowlistedBurnSwapWithAdmin

      call swap (Call @"Start") $ mkSingleOffer (SwapOffer [] [])
      call swap (Call @"Accept") (SwapId 0)

      call swap (Call @"Accept") (SwapId 0)
        & expectError swap errSwapFinished

      call swap (Call @"Cancel") (SwapId 0)
        & expectError swap errSwapFinished

  , nettestScenarioCaps "Operations with cancelled swap fail" $ do
      (swap, _) <- originateAllowlistedBurnSwapWithAdmin

      call swap (Call @"Start") $ mkSingleOffer (SwapOffer [] [])
      call swap (Call @"Cancel") (SwapId 0)

      call swap (Call @"Accept") (SwapId 0)
        & expectError swap errSwapCancelled

      call swap (Call @"Cancel") (SwapId 0)
        & expectError swap errSwapCancelled
  ]

swapIdChecks :: TestTree
swapIdChecks = testGroup "SwapIds"
  [ nettestScenarioCaps "Swap ids are properly assigned and can be worked with" $ do
      setup <- doFA2Setup
      let alice ::< bob ::< SNil = sAddresses setup
      let tokenId1 ::< tokenId2 ::< tokenId3 ::< SNil = sTokens setup
      (swap, admin) <- originateAllowlistedBurnSwapWithAdmin
      fa2 <- originateFA2 "fa2" setup [swap]

      withSender admin $
        call swap (Call @"Update_allowed") (mkAllowlistSimpleParam [fa2])

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
      (swap, _) <- originateAllowlistedBurnSwapWithAdmin

      call swap (Call @"Accept") (SwapId 0)
        & expectError swap errSwapNotExist
      call swap (Call @"Cancel") (SwapId 0)
        & expectError swap errSwapNotExist

      call swap (Call @"Start") $ mkSingleOffer (SwapOffer [] [])

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
      (swap, _) <- originateAllowlistedBurnSwapWithAdmin

      withSender alice $
        call swap (Call @"Start") $ mkSingleOffer (SwapOffer [] [])

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
        (swap, admin) <- originateAllowlistedBurnSwapWithAdmin

        withSender admin $
          call swap (Call @"Update_allowed") (mkAllowlistSimpleParam [fa2])

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
      (swap, admin) <- originateAllowlistedBurnSwapWithAdmin
      fa2_1 <- originateFA2 "fa2-1" setup [swap]
      fa2_2 <- originateFA2 "fa2-2" setup [swap]

      withSender admin $
        call swap (Call @"Update_allowed") (mkAllowlistSimpleParam [fa2_1, fa2_2])

      assertingBalanceDeltas fa2_1
        [ (alice, tokenId1) -: -100
        , (alice, tokenId2) -: -50
        , (nullAddress , tokenId3) -: 1
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

test_AdminChecks :: TestTree
test_AdminChecks =
  adminOwnershipTransferChecks originateAllowlistedBurnSwap

test_AllowlistUpdateAuthorization :: [TestTree]
test_AllowlistUpdateAuthorization =
  allowlistUpdateAuthorizationChecks originateAllowlistedBurnSwap

test_AllowlistChecks :: [TestTree]
test_AllowlistChecks = allowlistSimpleChecks
  AllowlistChecksSetup
  { allowlistCheckSetup = \fa2Setup -> do
      let alice ::< _ = sAddresses fa2Setup
      let tokenId ::< _ = sTokens fa2Setup
      (swap, admin) <- originateAllowlistedBurnSwapWithAdmin
      return (admin, swap, (alice, tokenId))

  , allowlistRestrictionsCases = fromList
      [ AllowlistRestrictionCase
        { allowlistError = errSwapOfferedNotAllowlisted
        , allowlistRunRestrictedAction = \(alice, tokenId) swap (fa2, _) ->
            withSender alice $
              call swap (Call @"Start") $ mkSingleOffer SwapOffer
                { assetsOffered = [mkFA2Assets fa2 [(tokenId, 1)]]
                , assetsRequested = []
                }
        }
      , AllowlistRestrictionCase
        { allowlistError = errSwapRequestedNotAllowlisted
        , allowlistRunRestrictedAction = \(alice, tokenId) swap (fa2, _) ->
            withSender alice $
              call swap (Call @"Start") $ mkSingleOffer SwapOffer
                { assetsOffered = []
                , assetsRequested = [mkFA2Assets fa2 [(tokenId, 1)]]
                }
        }
      ]

  , allowlistAlwaysIncluded = \_ -> []
  }

assertingBurnAddressUnchanged
  :: (MonadEmulated caps base m, HasCallStack)
  => TAddress AllowlistedBurnSwapEntrypoints
  -> m a
  -> m a
assertingBurnAddressUnchanged swapContract action = do
  initBurnAddress <- getBurnAddress swapContract
  res <- action
  finalBurnAddress <- getBurnAddress swapContract
  initBurnAddress @== finalBurnAddress
  return res
    where
      getBurnAddress c = do 
        storage <- fromVal @AllowlistedBurnSwapStorage <$> getStorage' c
        pure $ (burnAddress . burnSwapStorage) storage