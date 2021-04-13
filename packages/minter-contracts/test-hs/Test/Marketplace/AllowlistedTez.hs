-- | Tests on the allowlisted tez marketplace contract.
module Test.Marketplace.AllowlistedTez
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

test_Integrational :: TestTree
test_Integrational = testGroup "Integrational"
  [ -- Check that storage updates work
    nettestScenarioCaps "Simple performed sale" $ do
      setup <- doFA2Setup
      let alice ::< bob ::< SNil = sAddresses setup
      let tokenId1 ::< SNil = sTokens setup
      (market, admin) <- originateWithAdmin originateMarketplaceTezAllowlisted
      fa2 <- originateFA2 "fa2" setup [market]

      withSender (AddressResolved admin) $
        call market (Call @"Update_allowed") (mkAllowlistParam [fa2])

      let saleTokensParam = SaleTokenParamTez
            { tokenForSaleAddress = toAddress fa2
            , tokenForSaleTokenId = tokenId1
            }

      assertingBalanceDeltas fa2
        [ (alice, tokenId1) -: -1
        , (bob, tokenId1) -: 1
        ] $ do
          withSender (AddressResolved alice) $
            call market (Call @"Sell") InitSaleParamTez
              { salePrice = toMutez 1
              , saleTokensParam = saleTokensParam
              }

          withSender (AddressResolved bob) $
            transfer TransferData
              { tdTo = addressResolved market
              , tdAmount = toMutez 1
              , tdEntrypoint = ep "buy"
              , tdParameter = SaleParamTez
                { saleSeller = alice
                , tokens = saleTokensParam
                }
              }

  ]
