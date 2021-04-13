-- | Tests on the allowlisted tez english auction contract.
module Test.EnglishAuction.AllowlistedTez where

import Prelude hiding (swap)

import Test.Tasty (TestTree, testGroup)

import GHC.Exts (fromList)
import Lorentz.Value
import Morley.Nettest
import Morley.Nettest.Tasty

import Lorentz.Contracts.EnglishAuction.Allowlisted
import Lorentz.Contracts.EnglishAuction.Tez
import Test.EnglishAuction.Util
import Test.Util

import Test.Allowlisted
import Test.NonPausableSimpleAdmin

test_AdminChecks :: TestTree
test_AdminChecks =
  adminOwnershipTransferChecks originateAuctionTezAllowlisted

test_AllowlistUpdateAuthorization :: [TestTree]
test_AllowlistUpdateAuthorization =
  allowlistUpdateAuthorizationChecks originateAuctionTezAllowlisted

test_AllowlistChecks :: [TestTree]
test_AllowlistChecks = allowlistChecks
  AllowlistChecksSetup
  { allowlistCheckSetup = \fa2Setup -> do
      -- In this contract only alice can create auctions
      let alice ::< SNil = sAddresses fa2Setup
      let tokenId ::< SNil = sTokens fa2Setup
      contract <- originateAuctionTezAllowlisted alice
      return (alice, contract, (alice, tokenId))

  , allowlistRestrictionsCases = fromList
      [ AllowlistRestrictionCase
        { allowlistError = assetAddressNotAllowed
        , allowlistRunRestrictedAction = \(alice, tokenId) contract fa2 -> do
            now <- getNow
            withSender (AddressResolved alice) $
              call contract (Call @"Configure") $ (defConfigureParam :: ConfigureParam)
                { asset = [Tokens (toAddress fa2) [FA2Token tokenId 1]]
                , startTime = now
                }
        }
      ]

  , allowlistAlwaysIncluded = \_ -> []
  }

test_Integrational :: TestTree
test_Integrational = testGroup "Integrational"
  [ -- Check that storage updates work
    nettestScenarioCaps "Simple bid" $ do
      setup <- doFA2Setup
      let alice ::< bob ::< SNil = sAddresses setup
      let !SNil = sTokens setup
      auction <- originateAuctionTezAllowlisted alice
      fa2 <- originateFA2 "fa2" setup [auction]

      withSender (AddressResolved alice) $
        call auction (Call @"Update_allowed") (mkAllowlistParam [fa2])

      now <- getNow

      withSender (AddressResolved alice) $
        call auction (Call @"Configure") (defConfigureParam :: ConfigureParam)
          { startTime = now }

      withSender (AddressResolved bob) $
        transfer TransferData
          { tdTo = addressResolved auction
          , tdAmount = toMutez 3
          , tdEntrypoint = ep "bid"
          , tdParameter = AuctionId 0
          }

  ]
