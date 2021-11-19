-- | Tests on the swaps contract.
module Test.Swaps.Burn where

import Prelude hiding (swap)

import GHC.Exts (fromList)
import GHC.Integer (negateInteger)

import Hedgehog (Property, forAll, property)

import Test.Tasty (TestTree, testGroup)

import Morley.Nettest
import Morley.Nettest.Tasty (nettestScenarioCaps)

import Lorentz.Contracts.Swaps.Basic hiding (nextSwapId, swaps)
import Lorentz.Contracts.Swaps.Burn
import Lorentz.Contracts.Swaps.Allowlisted hiding (admin, allowlist)

import Test.Allowlisted
import Tezos.Address (unsafeParseAddress)
import Test.NonPausableSimpleAdmin
import Test.Swaps.Basic hiding (statusChecks, swapIdChecks, authorizationChecks, invalidFA2sChecks, complexCases)

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

hprop_Correct_final_balances_on_acceptance :: Property
hprop_Correct_final_balances_on_acceptance = 
  property $ do
   TestData{numOffers, token1Offer, token2Offer, token1Request, token2Request} <- forAll genTestData 
   clevelandProp  $ do
      setup <- doFA2Setup
      let admin ::< alice ::< SNil = sAddresses setup
      let tokenId1 ::< tokenId2 ::< SNil = sTokens setup
      swap <- originateAllowlistedBurnSwap admin
      fa2 <- originateFA2 "fa2" setup [swap]

      withSender admin $
        call swap (Call @"Update_allowed") (mkAllowlistSimpleParam [fa2])
      
      assertingBurnAddressUnchanged swap $ do 
        assertingBalanceDeltas fa2
          [ (admin, tokenId1) -: negateInteger (fromIntegral $ token1Offer * numOffers)
          , (admin, tokenId2) -: negateInteger (fromIntegral $ token2Offer * numOffers)
          , (nullAddress, tokenId1) -: (fromIntegral $ token1Request)
          , (nullAddress, tokenId2) -: (fromIntegral $ token2Request)
          , (alice, tokenId1) -: fromIntegral token1Offer - fromIntegral token1Request
          , (alice, tokenId2) -: fromIntegral token2Offer - fromIntegral token2Request
          ] $ do
            withSender admin $
              call swap (Call @"Start") $ mkNOffers numOffers SwapOffer
                { assetsOffered = [mkFA2Assets fa2 [(tokenId1, token1Offer), (tokenId2, token2Offer)]]
                , assetsRequested = [mkFA2Assets fa2 [(tokenId1, token1Request), (tokenId2, token2Request)]]
                }
            withSender alice $
              call swap (Call @"Accept") initSwapId
  
hprop_Correct_final_balances_on_cancel :: Property
hprop_Correct_final_balances_on_cancel = 
  property $ do
   TestData{numOffers, token1Offer, token2Offer, token1Request, token2Request} <- forAll genTestData 
   clevelandProp  $ do
      setup <- doFA2Setup
      let admin ::< alice ::< SNil = sAddresses setup
      let tokenId1 ::< tokenId2 ::< SNil = sTokens setup
      swap <- originateAllowlistedBurnSwap admin
      fa2 <- originateFA2 "fa2" setup [swap]

      withSender admin $
        call swap (Call @"Update_allowed") (mkAllowlistSimpleParam [fa2])
      
      assertingBurnAddressUnchanged swap $ do 
        assertingBalanceDeltas fa2
          [ (admin, tokenId1) -: 0
          , (admin, tokenId2) -: 0
          , (alice, tokenId1) -: 0
          , (alice, tokenId2) -: 0
          , (nullAddress, tokenId1) -: 0
          , (nullAddress, tokenId2) -: 0
          ] $ do
            withSender admin $
              call swap (Call @"Start") $ mkNOffers numOffers SwapOffer
                { assetsOffered = [mkFA2Assets fa2 [(tokenId1, token1Offer), (tokenId2, token2Offer)]]
                , assetsRequested = [mkFA2Assets fa2 [(tokenId1, token1Request), (tokenId2, token2Request)]]
                }
            withSender admin $
              call swap (Call @"Cancel") initSwapId
  
hprop_Correct_num_tokens_transferred_to_contract_on_start :: Property
hprop_Correct_num_tokens_transferred_to_contract_on_start = 
  property $ do
   TestData{numOffers, token1Offer, token2Offer} <- forAll genTestData 
   clevelandProp  $ do
     setup <- doFA2Setup
     let admin ::< SNil = sAddresses setup
     let tokenId1 ::< tokenId2 ::< SNil = sTokens setup
     swap <- originateAllowlistedBurnSwap admin
     fa2 <- originateFA2 "fa2" setup [swap]
     withSender admin $
        call swap (Call @"Update_allowed") (mkAllowlistSimpleParam [fa2])
     
     assertingBurnAddressUnchanged swap $ do 
       assertingBalanceDeltas fa2
         [ (admin, tokenId1) -: negateInteger (fromIntegral $ token1Offer * numOffers)
         , (admin, tokenId2) -: negateInteger (fromIntegral $ token2Offer * numOffers)
         ] $ do
           withSender admin $
             call swap (Call @"Start") $ mkNOffers numOffers SwapOffer
               { assetsOffered = [mkFA2Assets fa2 [(tokenId1, token1Offer), (tokenId2, token2Offer)]]
               , assetsRequested = [mkFA2Assets fa2 [(tokenId2, 1)]]
               }

hprop_Start_callable_by_admin_only :: Property
hprop_Start_callable_by_admin_only = 
  property $ do
   TestData{numOffers, token1Offer, token2Offer, token1Request, token2Request} <- forAll genTestData 
   clevelandProp  $ do
     setup <- doFA2Setup
     let admin ::< nonAdmin ::< SNil = sAddresses setup
     let tokenId1 ::< tokenId2 ::< SNil = sTokens setup
     swap <- originateAllowlistedBurnSwap admin
     fa2 <- originateFA2 "fa2" setup [swap]
     withSender nonAdmin 
       (call swap (Call @"Start") (mkNOffers numOffers SwapOffer
         { assetsOffered = [mkFA2Assets fa2 [(tokenId1, token1Offer), (tokenId2, token2Offer)]]
         , assetsRequested = [mkFA2Assets fa2 [(tokenId1, token1Request), (tokenId2, token2Request)]]
         }) & expectError swap errNotAdmin)


hprop_Contract_balance_goes_to_zero_when_sale_concludes :: Property
hprop_Contract_balance_goes_to_zero_when_sale_concludes = 
  property $ do
   TestData{numOffers, token1Offer, token2Offer, token1Request, token2Request} <- forAll genTestData 
   clevelandProp  $ do
     setup <- doFA2Setup
     let admin ::< SNil = sAddresses setup
     let tokenId1 ::< tokenId2 ::< SNil = sTokens setup
     swap <- originateAllowlistedBurnSwap admin
     let swapAddress = toAddress swap
     fa2 <- originateFA2 "fa2" setup [swap]
     assertingBurnAddressUnchanged swap $ do 
       withSender admin $
          call swap (Call @"Update_allowed") (mkAllowlistSimpleParam [fa2])
       assertingBalanceDeltas fa2
         [ (swapAddress, tokenId1) -: 0
         , (swapAddress, tokenId2) -: 0
         ] $ do
           withSender admin $
             call swap (Call @"Start") $ mkNOffers numOffers SwapOffer
               { assetsOffered = [mkFA2Assets fa2 [(tokenId1, token1Offer), (tokenId2, token2Offer)]]
               , assetsRequested = [mkFA2Assets fa2 [(tokenId1, token1Request), (tokenId2, token2Request)]]
               }
           withSender admin $
              replicateM_ (fromIntegral numOffers) $ do
                call swap (Call @"Accept") initSwapId

hprop_Burn_address_unchanged_correctly_tests_contract_storage :: Property
hprop_Burn_address_unchanged_correctly_tests_contract_storage  = 
  property $ do
    clevelandProp  $ do
      (swap, admin) <- originateChangeBurnAddressSwapWithAdmin
      assertingBurnAddressUnchanged swap (pure ())
      assertingBurnAddressChanged swap $ do 
         withSender admin $ 
           call swap (Call @"Change_burn_address") altBurnAddress

statusChecks :: TestTree
statusChecks = testGroup "Statuses"
  [ nettestScenarioCaps "Operations with accepted swap fail" $ do
      (swap, admin) <- originateAllowlistedBurnSwapWithAdmin
      
      withSender admin $
        call swap (Call @"Start") $ mkSingleOffer (SwapOffer [] [])
      call swap (Call @"Accept") initSwapId

      call swap (Call @"Accept") initSwapId
        & expectError swap errSwapFinished
      
      withSender admin
        (call swap (Call @"Cancel") initSwapId
          & expectError swap errSwapFinished)

  , nettestScenarioCaps "Operations with cancelled swap fail" $ do
      (swap, admin) <- originateAllowlistedBurnSwapWithAdmin
      
      withSender admin $
        call swap (Call @"Start") $ mkSingleOffer (SwapOffer [] [])
      withSender admin $
        call swap (Call @"Cancel") initSwapId

      call swap (Call @"Accept") initSwapId
        & expectError swap errSwapCancelled
      
      withSender admin $
        call swap (Call @"Cancel") initSwapId
          & expectError swap errSwapCancelled
  ]

swapIdChecks :: TestTree
swapIdChecks = testGroup "SwapIds"
  [ nettestScenarioCaps "Swap ids are properly assigned and can be worked with" $ do
      setup <- doFA2Setup
      let admin ::< alice ::< SNil = sAddresses setup
      let tokenId1 ::< tokenId2 ::< tokenId3 ::< SNil = sTokens setup
      swap <- originateAllowlistedBurnSwap admin
      fa2 <- originateFA2 "fa2" setup [swap]

      withSender admin $
        call swap (Call @"Update_allowed") (mkAllowlistSimpleParam [fa2])

      withSender admin $
        for_ [tokenId1, tokenId2, tokenId3] $ \tokenId ->
          call swap (Call @"Start") $ mkSingleOffer SwapOffer
            { assetsOffered = []
            , assetsRequested = [mkFA2Assets fa2 [(tokenId, 1)]]
            }

      assertingBalanceDeltas fa2
        [ (alice, tokenId1) -: -1
        , (alice, tokenId2) -:  0
        , (alice, tokenId3) -: -1
        ] $ do
          withSender alice $ do
            call swap (Call @"Accept") initSwapId
            call swap (Call @"Accept") (incrementSwapId $ incrementSwapId initSwapId)

  , nettestScenarioCaps "Accessing non-existing swap fails respectively" $ do
      (swap, admin) <- originateAllowlistedBurnSwapWithAdmin

      call swap (Call @"Accept") initSwapId
        & expectError swap errSwapNotExist
      
      withSender admin 
        (call swap (Call @"Cancel") initSwapId
          & expectError swap errSwapNotExist)
      
      withSender admin $ 
        call swap (Call @"Start") $ mkSingleOffer (SwapOffer [] [])

      call swap (Call @"Accept") (incrementSwapId initSwapId)
        & expectError swap errSwapNotExist

      withSender admin $ 
        call swap (Call @"Cancel") (incrementSwapId initSwapId)
          & expectError swap errSwapNotExist

      call swap (Call @"Accept") initSwapId
  ]


authorizationChecks :: TestTree
authorizationChecks = testGroup "Authorization checks"
  [ nettestScenarioCaps "Swap can be cancelled by seller only" $ do
      setup <- doFA2Setup
      let admin ::< alice ::< SNil = sAddresses setup
      let !SNil = sTokens setup
      swap <- originateAllowlistedBurnSwap admin

      withSender admin $
        call swap (Call @"Start") $ mkSingleOffer (SwapOffer [] [])

      call swap (Call @"Cancel") initSwapId
        & expectError swap errNotSwapSeller

      withSender alice $
        call swap (Call @"Cancel") initSwapId
        & expectError swap errNotSwapSeller
  ]

invalidFA2sChecks :: TestTree
invalidFA2sChecks = testGroup "Invalid FA2s"
  [ nettestScenarioCaps "Swap can be cancelled by seller only" $ do
      setup <- doFA2Setup
      let admin ::< SNil = sAddresses setup
      let tokenId1 ::< SNil = sTokens setup

      fakeFa2 <-
        TAddress . unTAddress <$>
        originateSimple "fake-fa2" ([] :: [Integer]) contractConsumer
      let nonExistingFa2 = TAddress $ unsafeParseAddress "tz1b7p3PPBd3vxmMHZkvtC61C7ttYE6g683F"
      let pseudoFa2s = [("fake FA2", fakeFa2), ("non existing FA2", nonExistingFa2)]

      for_ pseudoFa2s $ \(desc, fa2) -> do
        comment $ "Trying " <> desc
        swap <- originateAllowlistedBurnSwap admin

        withSender admin $
          call swap (Call @"Update_allowed") (mkAllowlistSimpleParam [fa2])

        comment "Checking offered FA2"
        withSender admin $
          call swap (Call @"Start") (mkSingleOffer SwapOffer
            { assetsOffered = [mkFA2Assets fa2 [(tokenId1, 1)]]
            , assetsRequested = []
            })
            & expectError swap errSwapOfferedFA2Invalid

        comment "Checking requested FA2"
        withSender admin $ do
          call swap (Call @"Start") $ mkSingleOffer SwapOffer
            { assetsOffered = []
            , assetsRequested = [mkFA2Assets fa2 [(tokenId1, 1)]]
            }

          call swap (Call @"Accept") initSwapId
            & expectError swap errSwapRequestedFA2Invalid
  ]

complexCases :: TestTree
complexCases = testGroup "Complex cases"
  [ nettestScenarioCaps "Multiple FA2s" $ do
      setup <- doFA2Setup
      let admin ::< alice ::< SNil = sAddresses setup
      let tokenId1 ::< tokenId2 ::< tokenId3 ::< SNil = sTokens setup
      swap <- originateAllowlistedBurnSwap admin
      fa2_1 <- originateFA2 "fa2-1" setup [swap]
      fa2_2 <- originateFA2 "fa2-2" setup [swap]

      withSender admin $
        call swap (Call @"Update_allowed") (mkAllowlistSimpleParam [fa2_1, fa2_2])

      assertingBalanceDeltas fa2_1
        [ (admin, tokenId1) -: -100
        , (admin, tokenId2) -: -50
        , (nullAddress , tokenId3) -: 1
        , (alice, tokenId1) -: 100
        , (alice, tokenId2) -: 50
        , (alice, tokenId3) -: -1
        ] $
        assertingBalanceDeltas fa2_2
        [ (admin, tokenId1) -: -1000
        , (alice, tokenId1) -: 1000
        ] $ do
          withSender admin $
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
          withSender alice $
            call swap (Call @"Accept") initSwapId

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
      let admin ::< _ = sAddresses fa2Setup
      let tokenId ::< _ = sTokens fa2Setup
      swap <- originateAllowlistedBurnSwap admin
      return (admin, swap, (admin, tokenId))

  , allowlistRestrictionsCases = fromList
      [ AllowlistRestrictionCase
        { allowlistError = errSwapOfferedNotAllowlisted
        , allowlistRunRestrictedAction = \(admin, tokenId) swap (fa2, _) ->
            withSender admin $
              call swap (Call @"Start") $ mkSingleOffer SwapOffer
                { assetsOffered = [mkFA2Assets fa2 [(tokenId, 1)]]
                , assetsRequested = []
                }
        }
      , AllowlistRestrictionCase
        { allowlistError = errSwapRequestedNotAllowlisted
        , allowlistRunRestrictedAction = \(admin, tokenId) swap (fa2, _) ->
            withSender admin $
              call swap (Call @"Start") $ mkSingleOffer SwapOffer
                { assetsOffered = []
                , assetsRequested = [mkFA2Assets fa2 [(tokenId, 1)]]
                }
        }
      ]

  , allowlistAlwaysIncluded = \_ -> []
  }

-- HELPERS -- 

assertingBurnAddressStatus
  :: (MonadEmulated caps base m, HasCallStack)
  => TAddress b
  -> m a
  -> (Address -> Address -> m ())
  -> m a
assertingBurnAddressStatus swapContract action changedStatus = do
  initBurnAddress <- getBurnAddress swapContract
  res <- action
  finalBurnAddress <- getBurnAddress swapContract
  initBurnAddress `changedStatus` finalBurnAddress
  return res
    where
      getBurnAddress c = do 
        storage <- fromVal @AllowlistedBurnSwapStorage <$> getStorage' c
        pure $ (burnAddress . burnSwapStorage) storage

assertingBurnAddressUnchanged 
  :: (MonadEmulated caps base m, HasCallStack)
  => TAddress b
  -> m a
  -> m a 
assertingBurnAddressUnchanged swapContract action = 
   assertingBurnAddressStatus swapContract action (@==)

assertingBurnAddressChanged
  :: (MonadEmulated caps base m, HasCallStack)
  => TAddress b
  -> m a
  -> m a 
assertingBurnAddressChanged swapContract action = 
   assertingBurnAddressStatus swapContract action (@/=)
