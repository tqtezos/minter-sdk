-- | Tests on the allowlisted FA2 english auction contract.
module Test.EnglishAuction.AllowlistedFA2 where

import Prelude hiding (swap)

import Test.Tasty (TestTree, testGroup)

import GHC.Exts (fromList)
import Lorentz.Contracts.Spec.FA2Interface (OperatorParam(..), UpdateOperator(..), theTokenId)
import Lorentz.Value
import Morley.Nettest
import Morley.Nettest.Tasty
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
test_AllowlistChecks = allowlistSimpleChecks
  AllowlistChecksSetup
  { allowlistCheckSetup = \fa2Setup -> do
      -- In this contract only admin can create auctions
      let admin ::< _ = sAddresses fa2Setup
      -- FA2 that is used as money we buy for in auction
      bidFA2 <- originateFA2 "bid-fa2" fa2Setup []
      let bidCurrency = BidCurrency (toAddress bidFA2) theTokenId
      contract <- originateAuctionFA2Allowlisted bidCurrency admin
      return (admin, contract, admin)

  , allowlistRestrictionsCases = fromList
      [ AllowlistRestrictionCase
        { allowlistError = assetNotAllowed
        , allowlistRunRestrictedAction =
          \admin contract (fa2, tokenId) -> do
            now <- getNow
            withSender admin $
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
      let tokenId ::< SNil = sTokens setup
      bidFA2 <- originateFA2 "bid-fa2" setup []
      auction <- originateAuctionFA2Allowlisted
        (BidCurrency (toAddress bidFA2) tokenId) alice
      fa2 <- originateFA2 "fa2" setup [auction]

      withSender bob $
        call bidFA2 (Call @"Update_operators") $ one $ AddOperator OperatorParam
          { opOwner = bob
          , opOperator = toAddress auction
          , opTokenId = tokenId
          }
      withSender alice $
        call auction (Call @"Update_allowed") (mkAllowlistSimpleParam [fa2])

      now <- getNow

      withSender alice $
        call auction (Call @"Configure") (defConfigureParam :: ConfigureParam)
          { startTime = now }

      withSender bob $
        call auction (Call @"Bid") (BidParam (AuctionId 0) 3)

  ]
