module Test.Marketplace.AllowlistedTokenFA2 where

import Test.Tasty (TestTree)

import GHC.Exts (fromList)
import Lorentz.Value
import Morley.Nettest

import Lorentz.Contracts.Marketplace.Allowlisted
import Lorentz.Contracts.Marketplace.FA2
import Lorentz.Contracts.Spec.FA2Interface (theTokenId)
import Test.Marketplace.Util
import Test.Util

import Test.Allowlisted
import Test.NonPausableSimpleAdmin

test_AdminChecks :: TestTree
test_AdminChecks =
  adminOwnershipTransferChecks originateMarketplaceAllowlistedToken

test_AllowlistUpdateAuthorization :: [TestTree]
test_AllowlistUpdateAuthorization =
  allowlistUpdateAuthorizationChecks originateMarketplaceAllowlistedToken

test_AllowlistChecks :: [TestTree]
test_AllowlistChecks = allowlistTokenChecks
  AllowlistChecksSetup
  { allowlistCheckSetup = \fa2Setup -> do
      let alice ::< _ = sAddresses fa2Setup
      market <- originateMarketplaceAllowlistedToken alice
      allowedFA2 <- originateFA2 "allowed-fa2" fa2Setup [market]
      return (alice, market, (alice, allowedFA2))

  , allowlistRestrictionsCases = fromList
      [ AllowlistRestrictionCase
        { allowlistError = saleTokenNotAllowed
        , allowlistRunRestrictedAction =
          \(alice, allowedFA2) market (fa2, tokenId) ->
            withSender alice $
              call market (Call @"Sell") SaleData
                { salePricePerToken = 1
                , saleToken = SaleToken
                  { fa2Address = toAddress fa2
                  , tokenId = tokenId
                  }
                , moneyToken = MoneyToken
                  { fa2Address = toAddress allowedFA2
                  , tokenId = theTokenId
                  }
                , tokenAmount = 1
                }
        }

      ]

  , allowlistAlwaysIncluded = \(_, allowedFA2) -> [(allowedFA2, theTokenId)]
  }
