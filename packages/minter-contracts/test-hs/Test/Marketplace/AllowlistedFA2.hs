-- | Tests on the allowlisted marketplace contract.
module Test.Marketplace.AllowlistedFA2 where

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
      market <- originateMarketplaceAllowlisted alice
      allowedFA2 <- originateFA2 "allowed-fa2" fa2Setup [market]
      return (alice, market, (alice, tokenId, allowedFA2))

  , allowlistRestrictionsCases = fromList
      [ AllowlistRestrictionCase
        { allowlistError = saleAddressNotAllowed
        , allowlistRunRestrictedAction = \(alice, tokenId, allowedFA2) market fa2 ->
            withSender alice $
              call market (Call @"Sell") SaleData
                { salePricePerToken = 1
                , saleToken = SaleToken
                    { fa2Address = toAddress fa2
                    , tokenId = tokenId
                    }
                , moneyToken = MoneyToken
                  {   fa2Address = toAddress allowedFA2
                    , tokenId = tokenId
                  }
                , tokenAmount = 1
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
      market <- originateMarketplaceAllowlisted alice
      fa2 <- originateFA2 "fa2" setup [market]

      withSender alice $
        call market (Call @"Update_allowed") (mkAllowlistParam [fa2])

      let saleToken = SaleToken
            { fa2Address = toAddress fa2
            , tokenId = tokenId1
            }

      let moneyToken = MoneyToken  
           { fa2Address = toAddress fa2
           , tokenId = tokenId2
           }

      assertingBalanceDeltas fa2
        [ (alice, tokenId1) -: -1
        , (alice, tokenId2) -: 10
        , (bob, tokenId1) -: 1
        , (bob, tokenId2) -: -10
        ] $ do
          withSender alice $
            call market (Call @"Sell") SaleData
              { salePricePerToken = 10
              , saleToken
              , moneyToken
              , tokenAmount = 1
              }
          withSender bob $
            call market (Call @"Buy") (SaleId 0)

  ]
