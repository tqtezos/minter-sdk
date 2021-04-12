-- | Tests on the allowlisted marketplace contract.
module Test.Marketplace.AllowlistedFA2
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
import Lorentz.Contracts.Marketplace.FA2
import Test.Marketplace.Util
import Test.Util

import Test.Allowlisted
import Test.NonPausableSimpleAdmin

test_AdminChecks :: TestTree
test_AdminChecks =
  adminOwnershipTransferChecks originateMarketplaceAllowlisted

test_AllowlistUpdateAuthorization :: [TestTree]
test_AllowlistUpdateAuthorization =
  allowlistUpdateAuthorizationChecks originateMarketplaceAllowlisted

test_AllowlistChecks :: [TestTree]
test_AllowlistChecks = allowlistChecks
  AllowlistChecksSetup
  { allowlistCheckSetup = \fa2Setup -> do
      let alice ::< SNil = sAddresses fa2Setup
      let tokenId ::< SNil = sTokens fa2Setup
      (swap, admin) <- originateWithAdmin originateMarketplaceAllowlisted
      allowedFA2 <- originateFA2 "allowed-fa2" fa2Setup [swap]
      return (admin, swap, (alice, tokenId, allowedFA2))

  , allowlistRestrictionsCases = fromList
      [ AllowlistRestrictionCase
        { allowlistError = saleAddressNotAllowed
        , allowlistRunRestrictedAction = \(alice, tokenId, allowedFA2) swap fa2 ->
            withSender (AddressResolved alice) $
              call swap (Call @"Sell") InitSaleParam
                { salePrice = 1
                , saleTokensParam = SaleTokenParam
                    { tokenForSaleAddress = toAddress fa2
                    , tokenForSaleTokenId = tokenId
                    , moneyTokenAddress = toAddress allowedFA2
                    , moneyTokenTokenId = tokenId
                    }
                }
        }

      , AllowlistRestrictionCase
        { allowlistError = moneyAddressNotAllowed
        , allowlistRunRestrictedAction = \(alice, tokenId, allowedFA2) swap fa2 ->
            withSender (AddressResolved alice) $
              call swap (Call @"Sell") InitSaleParam
                { salePrice = 1
                , saleTokensParam = SaleTokenParam
                    { tokenForSaleAddress = toAddress allowedFA2
                    , tokenForSaleTokenId = tokenId
                    , moneyTokenAddress = toAddress fa2
                    , moneyTokenTokenId = tokenId
                    }
                }
        }
      ]

  , allowlistAlwaysIncluded = \(_, _, allowedFA2) -> [allowedFA2]
  }
