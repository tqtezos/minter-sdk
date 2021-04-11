-- | Tests on the swaps contract.
module Test.Swaps.Allowlisted
  ( test_AdminChecks
  , test_AllowlistUpdateAuthorization
  , test_AllowlistChecks
  ) where

import Prelude hiding (swap)

import Test.Tasty (TestTree)

import GHC.Exts (fromList)
import Morley.Nettest

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
