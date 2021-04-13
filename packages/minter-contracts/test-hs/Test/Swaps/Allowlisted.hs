-- | Tests on the swaps contract.
module Test.Swaps.Allowlisted
  ( test_AdminChecks
  , test_AllowlistUpdateAuthorization
  , test_AllowlistChecks
  , test_Integrational
  ) where

import Prelude hiding (swap)

import Test.Tasty (TestTree, testGroup)

import GHC.Exts (fromList)
import Morley.Nettest
import Morley.Nettest.Tasty (nettestScenarioCaps)

import Lorentz.Contracts.Swaps.Allowlisted
import Lorentz.Contracts.Swaps.Basic
import Test.Swaps.Util
import Test.Util

import Test.Allowlisted
import Test.NonPausableSimpleAdmin

test_AdminChecks :: TestTree
test_AdminChecks =
  adminOwnershipTransferChecks originateAllowlistedSwap

test_AllowlistUpdateAuthorization :: [TestTree]
test_AllowlistUpdateAuthorization =
  allowlistUpdateAuthorizationChecks originateAllowlistedSwap

test_AllowlistChecks :: [TestTree]
test_AllowlistChecks = allowlistChecks
  AllowlistChecksSetup
  { allowlistCheckSetup = \fa2Setup -> do
      let alice ::< SNil = sAddresses fa2Setup
      let tokenId ::< SNil = sTokens fa2Setup
      (swap, admin) <- originateAllowlistedSwapWithAdmin
      return (admin, swap, (alice, tokenId))

  , allowlistRestrictionsCases = fromList
      [ AllowlistRestrictionCase
        { allowlistError = errSwapOfferedNotAllowlisted
        , allowlistRunRestrictedAction = \(alice, tokenId) swap fa2 ->
            withSender (AddressResolved alice) $
              call swap (Call @"Start") SwapOffer
                { assetsOffered = [mkFA2Assets fa2 [(tokenId, 1)]]
                , assetsRequested = []
                }
        }
      , AllowlistRestrictionCase
        { allowlistError = errSwapRequestedNotAllowlisted
        , allowlistRunRestrictedAction = \(alice, tokenId) swap fa2 ->
            withSender (AddressResolved alice) $
              call swap (Call @"Start") SwapOffer
                { assetsOffered = []
                , assetsRequested = [mkFA2Assets fa2 [(tokenId, 1)]]
                }
        }
      ]

  , allowlistAlwaysIncluded = \_ -> []
  }

test_Integrational :: TestTree
test_Integrational = testGroup "Integrational"
  [ -- Check that storage updates work
    nettestScenarioCaps "Simple accepted swap" $ do
      setup <- doFA2Setup
      let alice ::< bob ::< SNil = sAddresses setup
      let tokenId ::< SNil = sTokens setup
      (swap, admin) <- originateWithAdmin originateAllowlistedSwap
      fa2 <- originateFA2 "fa2" setup [swap]

      withSender (AddressResolved admin) $
        call swap (Call @"Update_allowed") (mkAllowlistParam [fa2])

      assertingBalanceDeltas fa2
        [ (alice, tokenId) -: -3
        , (bob, tokenId) -: 3
        ] $ do
          withSender (AddressResolved alice) $
            call swap (Call @"Start") SwapOffer
              { assetsOffered = [mkFA2Assets fa2 [(tokenId, 10)]]
              , assetsRequested = [mkFA2Assets fa2 [(tokenId, 7)]]
              }
          withSender (AddressResolved bob) $
            call swap (Call @"Accept") (SwapId 0)

  ]
