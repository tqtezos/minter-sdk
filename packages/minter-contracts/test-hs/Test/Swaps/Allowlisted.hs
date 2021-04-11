-- | Tests on the swaps contract.
module Test.Swaps.Allowlisted
  ( test_AdminChecks
  , test_AllowlistUpdateAuthorization
  , test_AllowlistChecks
  ) where

import Prelude hiding (swap)

import Test.Tasty (TestTree)

import Morley.Nettest
import Morley.Nettest.Tasty (nettestScenarioCaps)

import Lorentz.Contracts.NonPausableSimpleAdmin (errNotAdmin)
import Lorentz.Contracts.Swaps.Allowlisted
import Lorentz.Contracts.Swaps.Basic
import Lorentz.Value
import Test.Swaps.Util
import Test.Util

import Test.NonPausableSimpleAdmin

test_AdminChecks :: TestTree
test_AdminChecks =
  adminOwnershipTransferChecks @AllowlistedSwapEntrypoints originateAllowlistedSwap

test_AllowlistUpdateAuthorization :: [TestTree]
test_AllowlistUpdateAuthorization =
  [ nettestScenarioCaps "Can be updated by admin" $ do
      (swap, swapAdmin) <- originateAllowlistedSwapWithAdmin

      withSender (AddressResolved swapAdmin) $
        call swap (Call @"Update_allowed") mempty

  , nettestScenarioCaps "Cannot be updated by non-admins" $ do
      (swap, _swapAdmin) <- originateAllowlistedSwapWithAdmin
      user <- newAddress "user"

      withSender (AddressResolved user) $
        call swap (Call @"Update_allowed") mempty
        & expectError errNotAdmin
  ]

test_AllowlistChecks :: [TestTree]
test_AllowlistChecks =
  [ nettestScenarioCaps "Initially no FA2 works" $ do
      setup <- doSetup
      let alice ::< SNil = sAddresses setup
      let tokenId ::< SNil = sTokens setup
      (swap, _swapAdmin) <- originateAllowlistedSwapWithAdmin
      fa2 <- originateFA2 "fa2" setup swap

      withSender (AddressResolved alice) $ do
        call swap (Call @"Start") SwapOffer
          { assetsOffered = [mkFA2Assets fa2 [(tokenId, 1)]]
          , assetsRequested = []
          }
          & expectError errSwapOfferedNotAllowlisted

      withSender (AddressResolved alice) $ do
        call swap (Call @"Start") SwapOffer
          { assetsOffered = []
          , assetsRequested = [mkFA2Assets fa2 [(tokenId, 1)]]
          }
          & expectError errSwapRequestedNotAllowlisted

  , nettestScenarioCaps "Only FA2s that has been allowed work" $ do
      setup <- doSetup
      let alice ::< SNil = sAddresses setup
      let tokenId ::< SNil = sTokens setup
      (swap, swapAdmin) <- originateAllowlistedSwapWithAdmin
      fa2_1 <- originateFA2 "fa2-1" setup swap
      fa2_2 <- originateFA2 "fa2-2" setup swap

      withSender (AddressResolved swapAdmin) $
        call swap (Call @"Update_allowed") $ mkAllowlistParam [toAddress fa2_2]

      withSender (AddressResolved alice) $ do
        call swap (Call @"Start") SwapOffer
          { assetsOffered = [mkFA2Assets fa2_1 [(tokenId, 10)]]
          , assetsRequested = []
          }
          & expectError errSwapOfferedNotAllowlisted

        call swap (Call @"Start") SwapOffer
          { assetsOffered = [mkFA2Assets fa2_2 [(tokenId, 10)]]
          , assetsRequested = [mkFA2Assets fa2_2 [(tokenId, 10)]]
          }

  , nettestScenarioCaps "Permission can be revoked" $ do
      setup <- doSetup
      let alice ::< SNil = sAddresses setup
      let tokenId ::< SNil = sTokens setup
      (swap, swapAdmin) <- originateAllowlistedSwapWithAdmin
      fa2 <- originateFA2 "fa2" setup swap
      fa2_another <- originateFA2 "fa2" setup swap

      withSender (AddressResolved swapAdmin) $
        call swap (Call @"Update_allowed") $ mkAllowlistParam [toAddress fa2]
      withSender (AddressResolved swapAdmin) $
        call swap (Call @"Update_allowed") $ mkAllowlistParam [toAddress fa2_another]

      withSender (AddressResolved alice) $ do
        call swap (Call @"Start") SwapOffer
          { assetsOffered = []
          , assetsRequested = [mkFA2Assets fa2 [(tokenId, 1)]]
          }
          & expectError errSwapRequestedNotAllowlisted

  ]
