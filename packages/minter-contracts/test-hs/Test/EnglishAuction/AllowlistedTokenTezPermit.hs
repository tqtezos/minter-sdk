module Test.EnglishAuction.AllowlistedTokenTezPermit where

import Prelude hiding (swap)

import Test.Tasty (TestTree)

import GHC.Exts (fromList)
import Lorentz.Value
import Morley.Nettest

import Lorentz.Contracts.EnglishAuction.Allowlisted
import Lorentz.Contracts.EnglishAuction.Tez
import Lorentz.Contracts.EnglishAuction.TezPermit
import Test.EnglishAuction.Util
import Test.Util

import Test.Allowlisted
import Test.NonPausableSimpleAdmin

test_AdminChecks :: TestTree
test_AdminChecks =
  adminOwnershipTransferChecks originateAuctionTezPermitAllowlistedToken

test_AllowlistUpdateAuthorization :: [TestTree]
test_AllowlistUpdateAuthorization =
  allowlistUpdateAuthorizationChecks originateAuctionTezPermitAllowlistedToken

test_AllowlistChecks :: [TestTree]
test_AllowlistChecks = allowlistTokenChecks
  AllowlistChecksSetup
  { allowlistCheckSetup = \fa2Setup -> do
      -- In this contract only admin can create auctions
      let admin ::< _ = sAddresses fa2Setup
      contract <- originateAuctionTezPermitAllowlistedToken admin
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
