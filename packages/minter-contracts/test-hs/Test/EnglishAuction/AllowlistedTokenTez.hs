module Test.EnglishAuction.AllowlistedTokenTez where

import Prelude hiding (swap)

import Test.Tasty (TestTree)

import GHC.Exts (fromList)
import Lorentz.Value
import Morley.Nettest

import Lorentz.Contracts.EnglishAuction.Allowlisted
import Lorentz.Contracts.EnglishAuction.Common
import Lorentz.Contracts.EnglishAuction.Tez
import Test.EnglishAuction.Util
import Test.Util

import Test.Allowlisted
import Test.NonPausableSimpleAdmin

test_AdminChecks :: TestTree
test_AdminChecks =
  adminOwnershipTransferChecks originateAuctionTezAllowlistedToken

test_AllowlistUpdateAuthorization :: [TestTree]
test_AllowlistUpdateAuthorization =
  allowlistUpdateAuthorizationChecks originateAuctionTezAllowlistedToken

test_AllowlistChecks :: [TestTree]
test_AllowlistChecks = allowlistTokenChecks
  AllowlistChecksSetup
  { allowlistCheckSetup = \fa2Setup -> do
      -- In this contract only alice can create auctions
      let alice ::< _ = sAddresses fa2Setup
      contract <- originateAuctionTezAllowlistedToken alice
      return (alice, contract, alice)

  , allowlistRestrictionsCases = fromList
      [ AllowlistRestrictionCase
        { allowlistError = assetNotAllowed
        , allowlistRunRestrictedAction =
          \alice contract (fa2, tokenId) -> do
            now <- getNow
            withSender alice $
              call contract (Call @"Configure") $ (defConfigureParam :: ConfigureParam)
                { asset = [Tokens (toAddress fa2) [FA2Token tokenId 1]]
                , startTime = now
                }
        }
      ]

  , allowlistAlwaysIncluded = \_ -> []
  }
