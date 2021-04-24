module Test.Marketplace.AllowlistedTokenTez where

import Test.Tasty (TestTree)

import GHC.Exts (fromList)
import Lorentz.Value
import Morley.Nettest

import Lorentz.Contracts.Marketplace.Allowlisted
import Lorentz.Contracts.Marketplace.Tez
import Test.Marketplace.Util
import Test.Util

import Test.Allowlisted
import Test.NonPausableSimpleAdmin

test_AdminChecks :: TestTree
test_AdminChecks =
  adminOwnershipTransferChecks originateMarketplaceTezAllowlistedToken

test_AllowlistUpdateAuthorization :: [TestTree]
test_AllowlistUpdateAuthorization =
  allowlistUpdateAuthorizationChecks originateMarketplaceTezAllowlistedToken

test_AllowlistChecks :: [TestTree]
test_AllowlistChecks = allowlistTokenChecks
  AllowlistChecksSetup
  { allowlistCheckSetup = \fa2Setup -> do
      let alice ::< _ = sAddresses fa2Setup
      contract <- originateMarketplaceTezAllowlistedToken alice
      return (alice, contract, alice)

  , allowlistRestrictionsCases = fromList
      [ AllowlistRestrictionCase
        { allowlistError = saleTokenNotAllowed
        , allowlistRunRestrictedAction =
          \alice contract (fa2, tokenId) ->
            withSender alice $
              call contract (Call @"Sell") SaleDataTez
                { salePricePerToken = toMutez 1
                , saleToken = SaleToken(toAddress fa2) tokenId
                , tokenAmount = 1
                }
        }
      ]

  , allowlistAlwaysIncluded = \_ -> []
  }
