-- | Tests on the allowlisted tez marketplace contract.
module Test.Marketplace.AllowlistedTez
  ( test_AdminChecks
  , test_AllowlistUpdateAuthorization
  , test_AllowlistChecks
  ) where

import Prelude hiding (swap)

import Test.Tasty (TestTree)

import GHC.Exts (fromList)
import Lorentz.Value
import Morley.Nettest

import Lorentz.Contracts.Marketplace.Allowlisted
import Lorentz.Contracts.Marketplace.Tez
import Test.Marketplace.Util
import Test.Swaps.Util
import Test.Util

import Test.Allowlisted
import Test.NonPausableSimpleAdmin

test_AdminChecks :: TestTree
test_AdminChecks =
  adminOwnershipTransferChecks originateMarketplaceTezAllowlisted

test_AllowlistUpdateAuthorization :: [TestTree]
test_AllowlistUpdateAuthorization =
  allowlistUpdateAuthorizationChecks originateMarketplaceTezAllowlisted

test_AllowlistChecks :: [TestTree]
test_AllowlistChecks = allowlistChecks
  AllowlistChecksSetup
  { allowlistCheckSetup = \fa2Setup -> do
      let alice ::< SNil = sAddresses fa2Setup
      let tokenId ::< SNil = sTokens fa2Setup
      (contract, admin) <- originateWithAdmin originateMarketplaceTezAllowlisted
      return (admin, contract, (alice, tokenId))

  , allowlistRestrictionsCases = fromList
      [ AllowlistRestrictionCase
        { allowlistError = saleAddressNotAllowed
        , allowlistRunRestrictedAction = \(alice, tokenId) contract fa2 ->
            withSender (AddressResolved alice) $
              call contract (Call @"Sell") InitSaleParamTez
                { salePrice = toMutez 1
                , saleTokensParam = SaleTokenParamTez (toAddress fa2) tokenId
                }
        }
      ]

  , allowlistAlwaysIncluded = \_ -> []
  }
