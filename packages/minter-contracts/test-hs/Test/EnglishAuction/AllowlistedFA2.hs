-- | Tests on the allowlisted FA2 english auction contract.
module Test.EnglishAuction.AllowlistedFA2
  ( test_AdminChecks
  , test_AllowlistUpdateAuthorization
  , test_AllowlistChecks
  ) where

import Prelude hiding (swap)

import Test.Tasty (TestTree)

import GHC.Exts (fromList)
import Lorentz.Contracts.Spec.FA2Interface (theTokenId)
import Lorentz.Value
import Morley.Nettest
import Tezos.Address

import Lorentz.Contracts.EnglishAuction.Allowlisted
import Lorentz.Contracts.EnglishAuction.FA2
import Test.EnglishAuction.Util
import Test.Util

import Test.Allowlisted
import Test.NonPausableSimpleAdmin

dummyAddr :: Address
dummyAddr = unsafeParseAddress "tz1UuRPuMBuyMDxHAboCxPDG2ZxQFZdmHrWk"

test_AdminChecks :: TestTree
test_AdminChecks =
  adminOwnershipTransferChecks $
    originateAuctionFA2Allowlisted (BidCurrency dummyAddr theTokenId)

test_AllowlistUpdateAuthorization :: [TestTree]
test_AllowlistUpdateAuthorization =
  allowlistUpdateAuthorizationChecks $
    originateAuctionFA2Allowlisted (BidCurrency dummyAddr theTokenId)

test_AllowlistChecks :: [TestTree]
test_AllowlistChecks = allowlistChecks
  AllowlistChecksSetup
  { allowlistCheckSetup = \fa2Setup -> do
      -- In this contract only admin can create auctions
      let admin ::< SNil = sAddresses fa2Setup
      let tokenId ::< SNil = sTokens fa2Setup
      -- FA2 that is used as money we buy for in auction
      bidFA2 <- originateFA2 "bid-fa2" fa2Setup []
      let bidCurrency = BidCurrency (toAddress bidFA2) tokenId
      contract <- originateAuctionFA2Allowlisted bidCurrency admin
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
