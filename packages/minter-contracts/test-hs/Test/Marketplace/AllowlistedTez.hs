-- | Tests on the allowlisted tez marketplace contract.
module Test.Marketplace.AllowlistedTez where

import Test.Tasty (TestTree, testGroup)

import GHC.Exts (fromList)
import Lorentz.Value
import Morley.Nettest
import Morley.Nettest.Tasty (nettestScenarioCaps)

import Lorentz.Contracts.Marketplace.Allowlisted
import Lorentz.Contracts.Marketplace.Tez
import Test.Marketplace.Util
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
      contract <- originateMarketplaceTezAllowlisted alice
      return (alice, contract, (alice, tokenId))

  , allowlistRestrictionsCases = fromList
      [ AllowlistRestrictionCase
        { allowlistError = saleTokenNotAllowed
        , allowlistRunRestrictedAction = \(alice, tokenId) contract fa2 ->
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

test_Integrational :: TestTree
test_Integrational = testGroup "Integrational"
  [ -- Check that storage updates work
    nettestScenarioCaps "Simple performed sale" $ do
      setup <- doFA2Setup
      let alice ::< bob ::< SNil = sAddresses setup
      let tokenId1 ::< SNil = sTokens setup
      market <- originateMarketplaceTezAllowlisted alice
      fa2 <- originateFA2 "fa2" setup [market]

      withSender alice $
        call market (Call @"Update_allowed") (mkAllowlistParam [fa2])

      let saleToken = SaleToken
            { fa2Address = toAddress fa2
            , tokenId = tokenId1
            }

      assertingBalanceDeltas fa2
        [ (alice, tokenId1) -: -1
        , (bob, tokenId1) -: 1
        ] $ do
          withSender alice $
            call market (Call @"Sell") SaleDataTez
              { salePricePerToken = toMutez 1
              , saleToken = saleToken
              , tokenAmount = 1
              }

          withSender bob $
            transfer TransferData
              { tdTo = market
              , tdAmount = toMutez 1
              , tdEntrypoint = ep "buy"
              , tdParameter = SaleId 0 
              }

  ]
