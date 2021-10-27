-- | Tests on the swaps contract.
module Test.Swaps.AllowlistedFee where

import Prelude hiding (swap)

import Test.Tasty (TestTree, testGroup)

import Lorentz.Value
import Morley.Nettest
import Morley.Nettest.Tasty (nettestScenarioCaps)

import Lorentz.Contracts.Swaps.Basic hiding (SwapOffer, mkSingleOffer, mkNOffers)
import Lorentz.Contracts.Swaps.AllowlistedFee
import Test.Swaps.Util
import Test.Util

test_Integrational :: TestTree
test_Integrational = testGroup "Integrational"
  [ -- Check that storage updates work
    nettestScenarioCaps "Simple accepted swap without fee" $ do
      setup <- doFA2Setup
      let alice ::< bob ::< SNil = sAddresses setup
      let tokenId ::< SNil = sTokens setup
      (swap, admin) <- originateWithAdmin originateAllowlistedFeeSwap
      fa2 <- originateFA2 "fa2" setup [swap]

      withSender admin $
        call swap (Call @"Update_allowed") (mkAllowlistSimpleParam [fa2])

      assertingBalanceDeltas fa2
        [ (alice, tokenId) -: -3
        , (bob, tokenId) -: 3
        ] $ do
          withSender alice $
            call swap (Call @"Start") $ mkSingleOffer SwapOffer
              { assetsOffered = [mkFA2Assets fa2 [(tokenId, 10)]]
              , assetsRequested = ([mkFA2Assets fa2 [(tokenId, 7)]], 0)
              }
          withSender bob $
              call swap (Call @"Accept") (SwapId 0)
  ]

test_ContractSendsFee :: TestTree
test_ContractSendsFee = testGroup "Tests that contract sends fee"
  [ nettestScenarioCaps "Contract sends fee to seller" $ do
      setup <- doFA2Setup
      let alice ::< bob ::< SNil = sAddresses setup
      let tokenId ::< SNil = sTokens setup
      let amountRequested = toMutez 10
      (swap, admin) <- originateWithAdmin originateAllowlistedFeeSwap
      fa2 <- originateFA2 "fa2" setup [swap]
      sellerBalanceBefore <- getBalance alice
      buyerBalanceBefore <- getBalance bob

      withSender admin $
        call swap (Call @"Update_allowed") (mkAllowlistSimpleParam [fa2])
    
      withSender alice $
          call swap (Call @"Start") $ mkSingleOffer SwapOffer
            { assetsOffered = [mkFA2Assets fa2 [(tokenId, 10)]]
            , assetsRequested = ([mkFA2Assets fa2 [(tokenId, 7)]], amountRequested)
            }
      withSender bob $
          transfer TransferData
            { tdTo = swap
            , tdAmount = amountRequested
            , tdEntrypoint = ep "accept"
            , tdParameter = SwapId 0
            }
      
      sellerBalanceAfter <- getBalance alice
      buyerBalanceAfter <- getBalance bob
    
      sellerBalanceAfter @== sellerBalanceBefore + amountRequested
      buyerBalanceAfter @== buyerBalanceBefore - amountRequested 
  ]

test_BuyerMustSendFee :: TestTree
test_BuyerMustSendFee = testGroup "Tests that buyer must send fee"
  [ nettestScenarioCaps "Buyer must send fee to contract" $ do
      setup <- doFA2Setup
      let alice ::< bob ::< SNil = sAddresses setup
      let tokenId ::< SNil = sTokens setup
      let amountRequested = toMutez 10
      (swap, admin) <- originateWithAdmin originateAllowlistedFeeSwap
      fa2 <- originateFA2 "fa2" setup [swap]

      withSender admin $
        call swap (Call @"Update_allowed") (mkAllowlistSimpleParam [fa2])
    
      withSender alice $
          call swap (Call @"Start") $ mkSingleOffer SwapOffer
            { assetsOffered = [mkFA2Assets fa2 [(tokenId, 10)]]
            , assetsRequested = ([mkFA2Assets fa2 [(tokenId, 7)]], amountRequested)
            }
      withSender bob $
          transfer TransferData
            { tdTo = swap
            , tdAmount = toMutez 0
            , tdEntrypoint = ep "accept"
            , tdParameter = SwapId 0
            } 
          & expectError swap errNoXtzTransferred
  ]
