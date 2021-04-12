-- | Tests on the allowlisted tez english auction contract.
module Test.EnglishAuction.AllowlistedTez
  ( test_AdminChecks
  , test_AllowlistUpdateAuthorization
  , test_AllowlistChecks
  ) where

import Prelude hiding (swap)

import Test.Tasty (TestTree)

import GHC.Exts (fromList)
import Lorentz.Value
import Morley.Nettest

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
      -- In this contract only admin can create auctions
      let admin ::< SNil = sAddresses fa2Setup
      let tokenId ::< SNil = sTokens fa2Setup
      contract <- originateAuctionTezAllowlisted admin
      return (admin, contract, (admin, tokenId))

  , allowlistRestrictionsCases = fromList
      [ AllowlistRestrictionCase
        { allowlistError = assetAddressNotAllowed
        , allowlistRunRestrictedAction = \(admin, tokenId) contract fa2 -> do
            now <- getNow
            withSender (AddressResolved admin) $
              call contract (Call @"Configure") $ (defConfigureParam :: ConfigureParam)
                { asset = [Tokens (toAddress fa2) [FA2Token tokenId 1]]
                , startTime = now
                }
        }
      ]

  , allowlistAlwaysIncluded = \_ -> []
  }
