-- | Tests on the allowlisted tez english auction contract with permit.
module Test.EnglishAuction.AllowlistedTezPermit where

import Prelude hiding (swap)

import Test.Tasty (TestTree, testGroup)

import GHC.Exts (fromList)
import Lorentz.Value
import Morley.Nettest
import Morley.Nettest.Tasty

import Lorentz.Contracts.EnglishAuction.Allowlisted
import Lorentz.Contracts.EnglishAuction.Tez
import Lorentz.Contracts.EnglishAuction.TezPermit
import Test.EnglishAuction.Util
import Test.Util

import Test.Allowlisted
import Test.NonPausableSimpleAdmin

test_AdminChecks :: TestTree
test_AdminChecks =
  adminOwnershipTransferChecks originateAuctionTezPermitAllowlisted

test_AllowlistUpdateAuthorization :: [TestTree]
test_AllowlistUpdateAuthorization =
  allowlistUpdateAuthorizationChecks originateAuctionTezPermitAllowlisted

test_AllowlistChecks :: [TestTree]
test_AllowlistChecks = allowlistSimpleChecks
  AllowlistChecksSetup
  { allowlistCheckSetup = \fa2Setup -> do
      -- In this contract only admin can create auctions
      let admin ::< _ = sAddresses fa2Setup
      contract <- originateAuctionTezPermitAllowlisted admin
      return (admin, contract, admin)

  , allowlistRestrictionsCases = fromList
      [ AllowlistRestrictionCase
        { allowlistError = assetNotAllowed
        , allowlistRunRestrictedAction =
          \admin contract (fa2, tokenId) -> do
            now <- getNow
            withSender admin $
              call contract (Call @"Permit_configure") $ one $
                PermitConfigParam
                { config = defConfigureParam
                    { asset = [Tokens (toAddress fa2) [FA2Token tokenId 1]]
                    , startTime = now
                    }
                , optionalPermit = Nothing
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
      auction <- originateAuctionTezPermitAllowlisted alice
      fa2 <- originateFA2 "fa2" setup [auction]

      withSender alice $
        call auction (Call @"Update_allowed") (mkAllowlistSimpleParam [fa2])

      now <- getNow

      withSender alice $
        call auction (Call @"Permit_configure") $ one PermitConfigParam
          { config = (defConfigureParam :: ConfigureParam)
              { startTime = now }
          , optionalPermit = Nothing
          }

      withSender bob $
        transfer TransferData
          { tdTo = auction
          , tdAmount = toMutez 3
          , tdEntrypoint = ep "bid"
          , tdParameter = AuctionId 0
          }

  ]
