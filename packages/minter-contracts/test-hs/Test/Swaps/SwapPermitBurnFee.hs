-- | Tests on the swaps contract with offchain acceptance of a swap.
module Test.Swaps.SwapPermitBurnFee where

import Prelude hiding (swap, toStrict)

import qualified Data.Sized as Sized (toList)

import Hedgehog (Property, forAll, property)

import Michelson.Interpret.Pack

import GHC.Exts (fromList)
import GHC.Integer (negateInteger)

import qualified Lorentz.Contracts.Spec.FA2Interface as FA2I

import Test.Tasty (TestTree, testGroup)

import Morley.Nettest
import Morley.Nettest.Tasty (nettestScenarioCaps)

import Lorentz.Contracts.Swaps.Allowlisted
import Lorentz.Contracts.Swaps.AllowlistedFee hiding
  (errSwapOfferedNotAllowlisted, errSwapRequestedNotAllowlisted)
import Lorentz.Contracts.Swaps.Basic hiding (SwapOffer, mkNOffers, mkSingleOffer)
import Lorentz.Contracts.Swaps.Burn
import Lorentz.Contracts.Swaps.SwapPermit
import qualified Lorentz.Contracts.Swaps.SwapPermitBurnFee as SPBF
import Lorentz.Test (contractConsumer)
import Lorentz.Value

import qualified Test.Swaps.Basic as TestBasic

import Test.Allowlisted
import Test.NonPausableSimpleAdmin
import Test.Swaps.Util
import Test.Util

import Tezos.Address (unsafeParseAddress)
import Tezos.Crypto

----------------------------------------------------------------------------
-- Permit Tests
----------------------------------------------------------------------------

hprop_Sending_fake_permit_to_offchain_accept_fails :: Property
hprop_Sending_fake_permit_to_offchain_accept_fails =
  property $ do
    clevelandProp $ do
      setup <- doFA2Setup
      let admin ::< alice ::< SNil = sAddresses setup
      let tokenId1 ::< SNil = sTokens setup
      swap <- originateOffchainSwapBurnFee admin
      swapId <- (\(SwapId n) -> n) .
                SPBF.nextSwapId .
                SPBF.burnSwapStorage <$>
                getStorage' swap
      fa2 <- originateFA2 "fa2" setup [swap]
      withSender admin $
        call swap (Call @"Update_allowed") (mkAllowlistSimpleParam [fa2])
      withSender admin $ do
        call swap (Call @"Start") $ mkSingleOffer SwapOffer
          { assetsOffered = []
          , assetsRequested = ([mkFA2Assets fa2 [(tokenId1, 1)]], 0)
          }
      missignedBytes <- fst <$> mkPermitToForge swapId swap
      withSender admin $ do
        offchainAcceptForged alice swap
          & expectTransferFailure
            [failedWith $ constant ([mt|MISSIGNED|], missignedBytes)]

hprop_Offchain_accept_not_admin_submitted_fails :: Property
hprop_Offchain_accept_not_admin_submitted_fails =
  property $ do
    clevelandProp $ do
      setup <- doFA2Setup
      let admin ::< alice ::< SNil = sAddresses setup
      let tokenId1 ::< SNil = sTokens setup
      swap <- originateOffchainSwapBurnFee admin
      fa2 <- originateFA2 "fa2" setup [swap]
      withSender admin $
        call swap (Call @"Update_allowed") (mkAllowlistSimpleParam [fa2])
      withSender admin $ do
        call swap (Call @"Start") $ mkSingleOffer SwapOffer
          { assetsOffered = []
          , assetsRequested = ([mkFA2Assets fa2 [(tokenId1, 1)]], 0)
          }
      withSender alice $ do
        offchainAccept alice swap
          & expectTransferFailure [failedWith $ constant errNotAdmin]

hprop_Consecutive_offchain_accept_equals_iterative_accept :: Property
hprop_Consecutive_offchain_accept_equals_iterative_accept =
    property $ do
      TestBasic.TestData{numOffers,token1Offer, token2Offer, token1Request, token2Request} <- forAll TestBasic.genTestData
      clevelandProp $ do
        setup <- doFA2Setup @("addresses" :# 50) @("tokens" :# 2)
        let admin1 ::< admin2 ::< remainingAddresses = sAddresses setup
        let addresses = take (fromIntegral numOffers) (Sized.toList remainingAddresses)
        let tokenId1 ::< tokenId2 ::< SNil = sTokens setup
        swap1 <- originateOffchainSwapBurnFee admin1
        swap2 <- originateOffchainSwapBurnFee admin2
        fa2_1 <- originateFA2 "fa2_1" setup [swap1]
        fa2_2 <- originateFA2 "fa2_2" setup [swap2]
        withSender admin1 $
          call swap1 (Call @"Update_allowed") (mkAllowlistSimpleParam [fa2_1])
        withSender admin2 $
          call swap2 (Call @"Update_allowed") (mkAllowlistSimpleParam [fa2_2])
        withSender admin1 $ do
          call swap1 (Call @"Start") $ mkNOffers numOffers SwapOffer
               { assetsOffered = [mkFA2Assets fa2_1 [(tokenId1, token1Offer), (tokenId2, token2Offer)]]
               , assetsRequested = ([mkFA2Assets fa2_1 [(tokenId1, token1Request), (tokenId2, token2Request)]], 0)
               }
        withSender admin2 $ do
          call swap2 (Call @"Start") $ mkNOffers numOffers SwapOffer
               { assetsOffered = [mkFA2Assets fa2_2 [(tokenId1, token1Offer), (tokenId2, token2Offer)]]
               , assetsRequested = ([mkFA2Assets fa2_2 [(tokenId1, token1Request), (tokenId2, token2Request)]], 0)
               }
        withSender admin1 $ do
          offchainAcceptAllConsecutive addresses swap1
        withSender admin2 $ do
          offchainAcceptBatch addresses swap2

        swapStorage1 <- toVal . SPBF.burnSwapStorage <$> getStorage' swap1
        swapStorage2 <- toVal . SPBF.burnSwapStorage <$> getStorage' swap2
        swapStorage1 @== swapStorage2

----------------------------------------------------------------------------
-- Swap + Burn Tests Using Offchain_accept
----------------------------------------------------------------------------

hprop_Accepting_with_zero_balance_fails :: Property
hprop_Accepting_with_zero_balance_fails =
    property $ do
      clevelandProp $ do
          setup <- doFA2Setup
          let admin ::< SNil = sAddresses setup
          let tokenId1 ::< tokenId2 ::< SNil = sTokens setup
          swap <- originateOffchainSwapBurnFee admin
          fa2 <- originateFA2 "fa2" setup [swap]
          withSender admin $
            call swap (Call @"Update_allowed") (mkAllowlistSimpleParam [fa2])
          addressWithZeroBalance <- newAddress "test"
          withSender addressWithZeroBalance $
            call fa2 (Call @"Update_operators")
              [
                FA2I.AddOperator FA2I.OperatorParam
                  { opOwner = addressWithZeroBalance
                  , opOperator = toAddress swap
                  , opTokenId = tokenId2
                  }
              ]
          withSender admin $
            call swap (Call @"Start") $ mkSingleOffer SwapOffer
              { assetsOffered = [mkFA2Assets fa2 [(tokenId1, 10)]]
              , assetsRequested = ([mkFA2Assets fa2 [(tokenId2, 5)]], 0)
              }
          withSender admin
            (offchainAccept addressWithZeroBalance swap
              & expectTransferFailure
                [failedWith $ constant (errSwapRequestedFA2BalanceInvalid 5 0)])

hprop_Start_callable_by_admin_only :: Property
hprop_Start_callable_by_admin_only =
  property $ do
   TestBasic.TestData{numOffers, token1Offer, token2Offer, token1Request, token2Request} <- forAll TestBasic.genTestData
   clevelandProp $ do
     setup <- doFA2Setup
     let admin ::< nonAdmin ::< SNil = sAddresses setup
     let tokenId1 ::< tokenId2 ::< SNil = sTokens setup
     swap <- originateOffchainSwapBurnFee admin
     fa2 <- originateFA2 "fa2" setup [swap]
     withSender nonAdmin
       (call swap (Call @"Start") (mkNOffers numOffers SwapOffer
         { assetsOffered = [mkFA2Assets fa2 [(tokenId1, token1Offer), (tokenId2, token2Offer)]]
         , assetsRequested = ([mkFA2Assets fa2 [(tokenId1, token1Request), (tokenId2, token2Request)]], 0)
         }) & expectError errNotAdmin)

hprop_Correct_final_balances_on_acceptance :: Property
hprop_Correct_final_balances_on_acceptance =
  property $ do
   TestBasic.TestData{numOffers, token1Offer, token2Offer, token1Request, token2Request} <- forAll TestBasic.genTestData
   clevelandProp  $ do
      setup <- doFA2Setup
      let admin ::< alice ::< SNil = sAddresses setup
      let tokenId1 ::< tokenId2 ::< SNil = sTokens setup
      swap <- originateOffchainSwapBurnFee admin
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
                , assetsRequested = ([mkFA2Assets fa2 [(tokenId1, token1Request), (tokenId2, token2Request)]], 0)
                }
            withSender admin $
              offchainAccept alice swap

hprop_Correct_final_balances_on_cancel :: Property
hprop_Correct_final_balances_on_cancel =
  property $ do
   TestBasic.TestData{numOffers, token1Offer, token2Offer, token1Request, token2Request} <- forAll TestBasic.genTestData
   clevelandProp  $ do
      setup <- doFA2Setup
      let admin ::< alice ::< SNil = sAddresses setup
      let tokenId1 ::< tokenId2 ::< SNil = sTokens setup
      swap <- originateOffchainSwapBurnFee admin
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
                , assetsRequested = ([mkFA2Assets fa2 [(tokenId1, token1Request), (tokenId2, token2Request)]], 0)
                }
            withSender admin $
              call swap (Call @"Cancel") initSwapId

hprop_Correct_num_tokens_transferred_to_contract_on_start :: Property
hprop_Correct_num_tokens_transferred_to_contract_on_start =
  property $ do
   TestBasic.TestData{numOffers, token1Offer, token2Offer} <- forAll TestBasic.genTestData
   clevelandProp  $ do
     setup <- doFA2Setup
     let admin ::< SNil = sAddresses setup
     let tokenId1 ::< tokenId2 ::< SNil = sTokens setup
     swap <- originateOffchainSwapBurnFee admin
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
               , assetsRequested = ([mkFA2Assets fa2 [(tokenId2, 1)]], 0)
               }

hprop_Contract_balance_goes_to_zero_when_sale_concludes :: Property
hprop_Contract_balance_goes_to_zero_when_sale_concludes =
  property $ do
   TestBasic.TestData{numOffers, token1Offer, token2Offer, token1Request, token2Request} <- forAll TestBasic.genTestData
   clevelandProp  $ do
     setup <- doFA2Setup
     let admin ::< alice ::< SNil = sAddresses setup
     let tokenId1 ::< tokenId2 ::< SNil = sTokens setup
     swap <- originateOffchainSwapBurnFee admin
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
               , assetsRequested = ([mkFA2Assets fa2 [(tokenId1, token1Request), (tokenId2, token2Request)]], 0)
               }
           withSender admin $
              replicateM_ (fromIntegral numOffers) $ do
                offchainAccept alice swap

test_SwapPermitBurnFeeIntegrational :: TestTree
test_SwapPermitBurnFeeIntegrational = testGroup "Basic swap functionality"
  [ statusChecks
  , swapIdChecks
  , authorizationChecks
  , invalidFA2sChecks
  , complexCases
  ]


statusChecks :: TestTree
statusChecks = testGroup "Statuses"
  [ nettestScenarioCaps "Operations with accepted swap fail" $ do
      (swap, admin) <- originateOffchainSwapBurnFeeWithAdmin
      setup <- doFA2Setup
      let alice ::< SNil = sAddresses setup
      let !SNil = sTokens setup
      withSender admin $ do
        call swap (Call @"Start") $ mkSingleOffer (SwapOffer [] ([], 0))
        offchainAccept' alice swap

      withSender admin $
        offchainAccept' alice swap
        & expectError errSwapFinished

      withSender admin $
        offchainAccept' alice swap
        & expectError errSwapFinished

  , nettestScenarioCaps "Operations with cancelled swap fail" $ do
      (swap, admin) <- originateOffchainSwapBurnFeeWithAdmin
      setup <- doFA2Setup
      let alice ::< SNil = sAddresses setup
      let !SNil = sTokens setup
      withSender admin $
        call swap (Call @"Start") $ mkSingleOffer (SwapOffer [] ([], 0))
      withSender admin $
        call swap (Call @"Cancel") initSwapId

      withSender admin $
        offchainAccept' alice swap
        & expectError errSwapCancelled

      withSender admin $
        call swap (Call @"Cancel") initSwapId
          & expectError errSwapCancelled
  ]

swapIdChecks :: TestTree
swapIdChecks = testGroup "SwapIds"
  [ nettestScenarioCaps "Swap ids are properly assigned and can be worked with" $ do
      setup <- doFA2Setup
      let admin ::< alice ::< SNil = sAddresses setup
      let tokenId1 ::< tokenId2 ::< tokenId3 ::< SNil = sTokens setup
      swap <- originateOffchainSwapBurnFee admin
      fa2 <- originateFA2 "fa2" setup [swap]

      withSender admin $
        call swap (Call @"Update_allowed") (mkAllowlistSimpleParam [fa2])

      withSender admin $
        for_ [tokenId1, tokenId2, tokenId3] $ \tokenId ->
          call swap (Call @"Start") $ mkSingleOffer SwapOffer
            { assetsOffered = []
            , assetsRequested = ([mkFA2Assets fa2 [(tokenId, 1)]], 0)
            }

      assertingBalanceDeltas fa2
        [ (alice, tokenId1) -: -1
        , (alice, tokenId2) -:  0
        , (alice, tokenId3) -: -1
        ] $ do
          withSender admin $ do
            (\swapId -> offchainAcceptSwapId' swapId alice swap) (getSwapId initSwapId)
            (\swapId -> offchainAcceptSwapId' swapId alice swap) (getSwapId $ incrementSwapId $ incrementSwapId initSwapId)

  , nettestScenarioCaps "Accessing non-existing swap fails respectively" $ do
      (swap, admin) <- originateOffchainSwapBurnFeeWithAdmin
      setup <- doFA2Setup
      let alice ::< SNil = sAddresses setup
      let !SNil = sTokens setup
      withSender admin $
        offchainAccept' alice swap
          & expectError errSwapNotExist

      withSender admin
        (call swap (Call @"Cancel") initSwapId
          & expectError errSwapNotExist)

      withSender admin $ do
        call swap (Call @"Start") $ mkSingleOffer (SwapOffer [] ([], 0))

        (\swapId -> offchainAcceptSwapId' swapId alice swap) (getSwapId $ incrementSwapId initSwapId)
          & expectError errSwapNotExist

        call swap (Call @"Cancel") (incrementSwapId initSwapId)
          & expectError errSwapNotExist

        (\swapId -> offchainAcceptSwapId' swapId alice swap) (getSwapId initSwapId)
  ]


authorizationChecks :: TestTree
authorizationChecks = testGroup "Authorization checks"
  [ nettestScenarioCaps "Swap can be cancelled by seller only" $ do
      setup <- doFA2Setup
      let admin ::< alice ::< SNil = sAddresses setup
      let !SNil = sTokens setup
      swap <- originateOffchainSwapBurnFee admin

      withSender admin $
        call swap (Call @"Start") $ mkSingleOffer (SwapOffer [] ([], 0))

      call swap (Call @"Cancel") initSwapId
        & expectError errNotSwapSeller

      withSender alice $
        call swap (Call @"Cancel") initSwapId
        & expectError errNotSwapSeller
  ]

invalidFA2sChecks :: TestTree
invalidFA2sChecks = testGroup "Invalid FA2s"
  [ nettestScenarioCaps "Swap can be cancelled by seller only" $ do
      setup <- doFA2Setup
      let admin ::< alice ::< SNil = sAddresses setup
      let tokenId1 ::< SNil = sTokens setup

      fakeFa2 <- originateSimple "fake-fa2" ([] :: [Integer]) contractConsumer
      let nonExistingFa2 = ContractHandler "non-existing fa2" $
            unsafeParseAddress "tz1b7p3PPBd3vxmMHZkvtC61C7ttYE6g683F"
      let pseudoFa2s = [("fake FA2", fakeFa2), ("non existing FA2", nonExistingFa2)]

      for_ pseudoFa2s $ \(desc, fa2) -> do
        comment $ "Trying " <> desc
        swap <- originateOffchainSwapBurnFee admin

        withSender admin $
          call swap (Call @"Update_allowed") (mkAllowlistSimpleParam [fa2])

        comment "Checking offered FA2"
        withSender admin $
          call swap (Call @"Start") (mkSingleOffer SwapOffer
            { assetsOffered = [mkFA2Assets fa2 [(tokenId1, 1)]]
            , assetsRequested = ([], 0)
            })
            & expectError errSwapOfferedFA2Invalid

        comment "Checking requested FA2"
        withSender admin $ do
          call swap (Call @"Start") $ mkSingleOffer SwapOffer
            { assetsOffered = []
            , assetsRequested = ([mkFA2Assets fa2 [(tokenId1, 1)]], 0)
            }

          withSender admin
            (offchainAccept' alice swap)
            & expectError errSwapRequestedFA2Invalid
  ]

complexCases :: TestTree
complexCases = testGroup "Complex cases"
  [ nettestScenarioCaps "Multiple FA2s" $ do
      setup <- doFA2Setup
      let admin ::< alice ::< SNil = sAddresses setup
      let tokenId1 ::< tokenId2 ::< tokenId3 ::< SNil = sTokens setup
      swap <- originateOffchainSwapBurnFee admin
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
                  ([ mkFA2Assets fa2_1
                    [ (tokenId3, 1)
                    ]
                  ], 0)
              }
          withSender admin $
            offchainAccept' alice swap

  ]

----------------------------------------------------------------------------
-- Swap + Fee + Burn Tests Using normal Accept
----------------------------------------------------------------------------
test_Integrational :: TestTree
test_Integrational = testGroup "Integrational"
  [ -- Check that storage updates work
    nettestScenarioCaps "Simple accepted swap without fee" $ do
      setup <- doFA2Setup
      let admin ::< alice ::< SNil = sAddresses setup
      let tokenId ::< SNil = sTokens setup
      swap <- originateOffchainSwapBurnFee admin
      fa2 <- originateFA2 "fa2" setup [swap]

      withSender admin $
        call swap (Call @"Update_allowed") (mkAllowlistSimpleParam [fa2])

      assertingBalanceDeltas fa2
        [ (admin, tokenId) -: -10
        , (nullAddress, tokenId) -: 7
        , (alice, tokenId) -: 3
        ] $ do
          withSender admin $
            call swap (Call @"Start") $ mkSingleOffer SwapOffer
              { assetsOffered = [mkFA2Assets fa2 [(tokenId, 10)]]
              , assetsRequested = ([mkFA2Assets fa2 [(tokenId, 7)]], 0)
              }
          withSender alice $
              call swap (Call @"Accept") initSwapId
  ]

test_ContractSendsFee :: TestTree
test_ContractSendsFee = testGroup "Tests that contract sends fee"
  [ nettestScenarioCaps "Contract sends fee to seller" $ do
      setup <- doFA2Setup
      let admin ::< alice ::< SNil = sAddresses setup
      let tokenId ::< SNil = sTokens setup
      let amountRequested = toMutez 10
      swap <- originateOffchainSwapBurnFee admin
      fa2 <- originateFA2 "fa2" setup [swap]
      sellerBalanceBefore <- getBalance admin
      buyerBalanceBefore <- getBalance alice

      withSender admin $
        call swap (Call @"Update_allowed") (mkAllowlistSimpleParam [fa2])

      withSender admin $
          call swap (Call @"Start") $ mkSingleOffer SwapOffer
            { assetsOffered = [mkFA2Assets fa2 [(tokenId, 10)]]
            , assetsRequested = ([mkFA2Assets fa2 [(tokenId, 7)]], amountRequested)
            }
      withSender alice $
          transfer TransferData
            { tdTo = swap
            , tdAmount = amountRequested
            , tdEntrypoint = ep "accept"
            , tdParameter = initSwapId
            }

      sellerBalanceAfter <- getBalance admin
      buyerBalanceAfter <- getBalance alice

      sellerBalanceAfter @== sellerBalanceBefore + amountRequested
      buyerBalanceAfter @== buyerBalanceBefore - amountRequested
  ]

test_BuyerMustSendFee :: TestTree
test_BuyerMustSendFee = testGroup "Tests that buyer must send fee"
  [ nettestScenarioCaps "Buyer must send fee to contract" $ do
      setup <- doFA2Setup
      let admin ::< alice ::< SNil = sAddresses setup
      let tokenId ::< SNil = sTokens setup
      let amountRequested = toMutez 10
      swap <- originateOffchainSwapBurnFee admin
      fa2 <- originateFA2 "fa2" setup [swap]

      withSender admin $
        call swap (Call @"Update_allowed") (mkAllowlistSimpleParam [fa2])

      withSender admin $
          call swap (Call @"Start") $ mkSingleOffer SwapOffer
            { assetsOffered = [mkFA2Assets fa2 [(tokenId, 10)]]
            , assetsRequested = ([mkFA2Assets fa2 [(tokenId, 7)]], amountRequested)
            }
      withSender alice $
          transfer TransferData
            { tdTo = swap
            , tdAmount = toMutez 0
            , tdEntrypoint = ep "accept"
            , tdParameter = initSwapId
            }
          & expectError errNoXtzTransferred
  ]

----------------------------------------------------------------------------
-- Admin and Allowlist Checks
----------------------------------------------------------------------------


test_AdminChecks :: TestTree
test_AdminChecks =
  adminOwnershipTransferChecks originateOffchainSwapBurnFee

test_AllowlistUpdateAuthorization :: [TestTree]
test_AllowlistUpdateAuthorization =
  allowlistUpdateAuthorizationChecks originateOffchainSwapBurnFee

test_AllowlistChecks :: [TestTree]
test_AllowlistChecks = allowlistSimpleChecks
  AllowlistChecksSetup
  { allowlistCheckSetup = \fa2Setup -> do
      let admin ::< _ = sAddresses fa2Setup
      let tokenId ::< _ = sTokens fa2Setup
      swap <- originateOffchainSwapBurnFee admin
      return (admin, swap, (admin, tokenId))

  , allowlistRestrictionsCases = fromList
      [ AllowlistRestrictionCase
        { allowlistError = errSwapOfferedNotAllowlisted
        , allowlistRunRestrictedAction = \(admin, tokenId) swap (fa2, _) ->
            withSender admin $
              call swap (Call @"Start") $ mkSingleOffer SwapOffer
                { assetsOffered = [mkFA2Assets fa2 [(tokenId, 1)]]
                , assetsRequested = ([], 0)
                }
        }
      , AllowlistRestrictionCase
        { allowlistError = errSwapRequestedNotAllowlisted
        , allowlistRunRestrictedAction = \(admin, tokenId) swap (fa2, _) ->
            withSender admin $
              call swap (Call @"Start") $ mkSingleOffer SwapOffer
                { assetsOffered = []
                , assetsRequested = ([mkFA2Assets fa2 [(tokenId, 1)]], 0)
                }
        }
      ]

  , allowlistAlwaysIncluded = \_ -> []
  }


----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- N addresses accept all N assets in a sale conseutively, and then all N are confirmed
offchainAcceptAllConsecutive :: (HasCallStack, MonadEmulated caps base m) => [Address] -> ContractHandler SPBF.PermitSwapBurnFeeEntrypoints st -> m ()
offchainAcceptAllConsecutive addresses contract = do
  forM_ addresses $ \buyer -> do
      offchainAccept buyer contract

offchainAcceptBatch :: (HasCallStack, MonadEmulated caps base m) => [Address] -> ContractHandler SPBF.PermitSwapBurnFeeEntrypoints st -> m ()
offchainAcceptBatch buyers contract = do
  param <- forM buyers $ \buyer -> do
    buyerPK <- getPublicKey buyer
    unsigned <- mkPermitToSign swapId contract
    signature <- signBytes unsigned buyer
    return OffchainAcceptParam {
        swapId = swapId
      , permit = Permit
          {
            signerKey = buyerPK
          , signature = signature
          }
      }
  call contract (Call @"Offchain_accept") (toList param)
  where swapId = 1

mkPermitToForge :: (HasCallStack, MonadEmulated caps base m) => Natural -> ContractHandler SPBF.PermitSwapBurnFeeEntrypoints st -> m (ByteString, PublicKey)
mkPermitToForge swapId contract = do
  aliasAddress <- newAddress "forged"
  aliasPK <- getPublicKey aliasAddress
  unsignedPermit <- mkPermitToSign swapId contract
  pure (unsignedPermit, aliasPK)

mkPermitToSign :: (HasCallStack, MonadEmulated caps base m) => Natural -> ContractHandler SPBF.PermitSwapBurnFeeEntrypoints st -> m ByteString
mkPermitToSign swapId contract = do
  marketplaceChainId <- getChainId
  let unsigned = packValue' $ toVal ((marketplaceChainId, contractAddress), (0 :: Natural, swapIdHash))
  pure unsigned
  where swapIdHash = blake2b $ packValue' $ toVal swapId
        contractAddress = toAddress contract

offchainAcceptSwapId :: (HasCallStack, MonadEmulated caps base m) => Natural -> Address -> ContractHandler SPBF.PermitSwapBurnFeeEntrypoints st -> m ()
offchainAcceptSwapId swapId buyer contract = do
  buyerPK <- getPublicKey buyer
  unsigned <- mkPermitToSign swapId contract
  signature <- signBytes unsigned buyer
  call contract (Call @"Offchain_accept")
    [OffchainAcceptParam
      {
        swapId = swapId
      , permit = Permit
          {
            signerKey = buyerPK
          , signature = signature
          }
      }
    ]

offchainAccept :: (HasCallStack, MonadEmulated caps base m) => Address -> ContractHandler SPBF.PermitSwapBurnFeeEntrypoints st -> m ()
offchainAccept = offchainAcceptSwapId 1

mkPermitToSign' :: (HasCallStack, MonadNettest caps base m) => Natural -> ContractHandler SPBF.PermitSwapBurnFeeEntrypoints st -> m ByteString
mkPermitToSign' swapId contract = do
  marketplaceChainId <- getChainId
  let unsigned = packValue' $ toVal ((marketplaceChainId, contractAddress), (0 :: Natural, swapIdHash))
  pure unsigned
  where swapIdHash = blake2b $ packValue' $ toVal swapId
        contractAddress = toAddress contract

offchainAcceptSwapId' :: (HasCallStack, MonadNettest caps base m) => Natural -> Address -> ContractHandler SPBF.PermitSwapBurnFeeEntrypoints st -> m ()
offchainAcceptSwapId' swapId buyer contract = do
  buyerPK <- getPublicKey buyer
  unsigned <- mkPermitToSign' swapId contract
  signature <- signBytes unsigned buyer
  call contract (Call @"Offchain_accept")
    [OffchainAcceptParam
      {
        swapId = swapId
      , permit = Permit
          {
            signerKey = buyerPK
          , signature = signature
          }
      }
    ]

offchainAccept' :: (HasCallStack, MonadNettest caps base m) => Address -> ContractHandler SPBF.PermitSwapBurnFeeEntrypoints st -> m ()
offchainAccept' = offchainAcceptSwapId' 1


offchainAcceptForged :: (HasCallStack, MonadEmulated caps base m) => Address -> ContractHandler SPBF.PermitSwapBurnFeeEntrypoints st -> m ByteString
offchainAcceptForged buyer contract = do
  (unsigned, forgedPK) <- mkPermitToForge swapId contract
  signature <- signBytes unsigned buyer
  (\() -> unsigned) <$> call contract (Call @"Offchain_accept")
    [OffchainAcceptParam
      {
        swapId = swapId
      , permit = Permit
          {
            signerKey = forgedPK
          , signature = signature
          }
      }
    ]
  where swapId = 1

assertingBurnAddressStatus
  :: (MonadEmulated caps base m, HasCallStack)
  => ContractHandler b SPBF.AllowlistedBurnSwapFeeStorage
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
      getBurnAddress c =
        SPBF.burnAddress . SPBF.burnSwapStorage <$> getStorage' c

assertingBurnAddressUnchanged
  :: (MonadEmulated caps base m, HasCallStack)
  => ContractHandler b SPBF.AllowlistedBurnSwapFeeStorage
  -> m a
  -> m a
assertingBurnAddressUnchanged swapContract action =
   assertingBurnAddressStatus swapContract action (@==)

assertingBurnAddressChanged
  :: (MonadEmulated caps base m, HasCallStack)
  => ContractHandler b SPBF.AllowlistedBurnSwapFeeStorage
  -> m a
  -> m a
assertingBurnAddressChanged swapContract action =
   assertingBurnAddressStatus swapContract action (@/=)
