-- | Tests on the allowlisted marketplace contract.
module Test.Marketplace.AllowlistedFA2
  ( test_AdminChecks
  , test_AllowlistUpdateAuthorization
  , test_AllowlistChecks
  , test_Integrational
  ) where

import Test.Tasty (TestTree, testGroup)

import GHC.Exts (fromList)
import Lorentz.Value
import Morley.Nettest
import Morley.Nettest.Tasty (nettestScenarioCaps)

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
      (market, admin) <- originateWithAdmin originateMarketplaceAllowlisted
      allowedFA2 <- originateFA2 "allowed-fa2" fa2Setup [market]
      return (admin, market, (alice, tokenId, allowedFA2))

  , allowlistRestrictionsCases = fromList
      [ AllowlistRestrictionCase
        { allowlistError = saleAddressNotAllowed
        , allowlistRunRestrictedAction = \(alice, tokenId, allowedFA2) market fa2 ->
            withSender (AddressResolved alice) $
              call market (Call @"Sell") InitSaleParam
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
        , allowlistRunRestrictedAction = \(alice, tokenId, allowedFA2) market fa2 ->
            withSender (AddressResolved alice) $
              call market (Call @"Sell") InitSaleParam
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

test_Integrational :: TestTree
test_Integrational = testGroup "Integrational"
  [ -- Check that storage updates work
    nettestScenarioCaps "Simple performed sale" $ do
      setup <- doFA2Setup
      let alice ::< bob ::< SNil = sAddresses setup
      let tokenId1 ::< tokenId2 ::< SNil = sTokens setup
      (market, admin) <- originateWithAdmin originateMarketplaceAllowlisted
      fa2 <- originateFA2 "fa2" setup [market]

      withSender (AddressResolved admin) $
        call market (Call @"Update_allowed") (mkAllowlistParam [fa2])

      let saleTokensParam = SaleTokenParam
            { tokenForSaleAddress = toAddress fa2
            , tokenForSaleTokenId = tokenId1
            , moneyTokenAddress = toAddress fa2
            , moneyTokenTokenId = tokenId2
            }

      assertingBalanceDeltas fa2
        [ (alice, tokenId1) -: -1
        , (alice, tokenId2) -: 10
        , (bob, tokenId1) -: 1
        , (bob, tokenId2) -: -10
        ] $ do
          withSender (AddressResolved alice) $
            call market (Call @"Sell") InitSaleParam
              { salePrice = 10
              , saleTokensParam
              }
          withSender (AddressResolved bob) $
            call market (Call @"Buy") SaleParam
              { saleSeller = alice
              , tokens = saleTokensParam
              }

  ]
