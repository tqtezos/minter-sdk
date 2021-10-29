-- | Tests on the swaps contract.
module Test.Swaps.Burn where

import Prelude hiding (swap)

import Test.Tasty (TestTree, testGroup)

import Morley.Nettest
import Morley.Nettest.Tasty (nettestScenarioCaps)

import Lorentz.Contracts.Swaps.Basic
import Lorentz.Contracts.Swaps.Burn
import Test.Swaps.Util
import Test.Util



test_Integrational :: TestTree
test_Integrational = testGroup "Integrational"
  [ -- Check that storage updates work
    nettestScenarioCaps "Simple accepted swap" $ do
      setup <- doFA2Setup
      let alice ::< bob ::< SNil = sAddresses setup
      let tokenId ::< SNil = sTokens setup
      (swap, admin) <- originateWithAdmin originateAllowlistedBurnSwap
      fa2 <- originateFA2 "fa2" setup [swap]

      withSender admin $
        call swap (Call @"Update_allowed") (mkAllowlistSimpleParam [fa2])

      assertingBalanceDeltas fa2
        [ (alice, tokenId) -: -10
        , (bob, tokenId) -: 3
        , (nullAddress, tokenId) -: 7
        ] $ do
          withSender alice $
            call swap (Call @"Start") SwapOffer
              { assetsOffered = [mkFA2Assets fa2 [(tokenId, 10)]]
              , assetsRequested = [mkFA2Assets fa2 [(tokenId, 7)]]
              }
          withSender bob $
            call swap (Call @"Accept") (SwapId 0)

  ]
