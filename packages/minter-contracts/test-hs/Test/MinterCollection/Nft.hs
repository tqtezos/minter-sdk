{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedLists #-}

-- | Tests for NFT multi-asset contract
module Test.MinterCollection.Nft where

import Prelude hiding (swap)

-- import GHC.Exts (fromList)
import Test.Tasty (TestTree, testGroup)

import qualified Lorentz.Contracts.FA2 as FA2 -- (TokenMetadata(..))
import Lorentz.Contracts.Spec.FA2Interface (OperatorParam(..), TokenId(..), UpdateOperator(..), mkTokenMetadata)
-- import Lorentz.Value (BigMap(..))
-- import Lorentz.Errors

import Michelson.Text (unsafeMkMText)
import Morley.Nettest
import Morley.Nettest.Tasty -- (nettestScenarioCaps)

import Lorentz.Contracts.MinterCollection.Nft.Contract (nftContract)
import Lorentz.Contracts.MinterCollection.Nft.Types

import Test.SimpleAdmin
import Test.Util


originateNft
  :: MonadNettest caps base m
  => NftStorage
  -> m (ContractHandler NftEntrypoints NftStorage)
originateNft storage =
  originateSimple "nft-multi-asset" storage nftContract

-- Test SimpleAdmin admin ownership transfer
test_AdminChecks :: TestTree
test_AdminChecks =
  adminOwnershipTransferChecks @NftEntrypoints  @NftStorage
    (\admin ->
      originateNft
        (exampleNftStorageWithAdmin admin)
    )


-- type nft_asset_entrypoints =
--   | Assets of fa2_entry_points
--   | Mint of mint_tokens_param
--   | Burn of (token_id * bytes)
--   | Update_metadata of (token_metadata list)
--   | Admin of admin_entrypoints

test_Integrational :: TestTree
test_Integrational = testGroup "Integrational"
  [

      -- withSender bob $
      --   transfer TransferData
      --     { tdTo = auction
      --     , tdAmount = toMutez 3
      --     , tdEntrypoint = ep "bid"
      --     , tdParameter = AuctionId 0
      --     }

    -- storage updates work
    -- - Mint (alice)
    -- - Update_metadata (alice)
    -- - Update_operators (bob -> alice)
    -- - Burn (alice)
    -- (emulated for easy access to storage)
    nettestScenarioOnEmulatorCaps "Mint update burn: storage" $ do
      setup <- doFA2Setup @("addresses" :# 2) @("tokens" :# 0)
      let alice ::< bob ::< SNil = sAddresses setup
      nft <- originateNft (exampleNftStorageWithAdmin alice)

      -- mint to bob
      let tokenMetadata0 = mkTokenMetadata "nft-symbol-0" "nft-name-0" "12"
      let tokenMetadata0' = FA2.TokenMetadata
            { tokenId = TokenId 0
            , tokenInfo = tokenMetadata0
            }
      withSender alice $
        call nft (Call @"Mint") [MintTokenParam
          { token_metadata = tokenMetadata0'
          , owner = bob
          }]

      postMintStorage <- getStorage' nft
      postMintStorage @== (exampleNftStorageWithAdmin alice) {
        assets = exampleNftTokenStorage {
            ledger = [(TokenId 0, bob)]
          , next_token_id = TokenId 1
          , token_metadata = [(TokenId 0, tokenMetadata0')]
          } }

      -- bob can't update metadata, because not admin
      let tokenMetadata1 = mkTokenMetadata "nft-symbol-1" "nft-name-1" "24"
      let tokenMetadata1' = FA2.TokenMetadata
            { tokenId = TokenId 0
            , tokenInfo = tokenMetadata1
            }
      withSender bob $
        call nft (Call @"Update_metadata") [tokenMetadata1']
          & expectError (unsafeMkMText "NOT_AN_ADMIN")

      -- alice (as admin) can update metadata
      withSender alice $
        call nft (Call @"Update_metadata") [FA2.TokenMetadata
          { tokenId = TokenId 0
          , tokenInfo = tokenMetadata1
          }]

      -- bob makes alice an operator
      withSender bob $
        call nft (Call @"Update_operators") [AddOperator $ OperatorParam
          { opOwner = bob
          , opOperator = alice
          , opTokenId = TokenId 0
          }]

      -- alice is now an operator, so can burn
      withSender alice $
        call nft (Call @"Burn") (TokenId 0, "nft-symbol-1")

      postBurnStorage <- getStorage' nft
      postBurnStorage @== (exampleNftStorageWithAdmin alice) {
        assets = exampleNftTokenStorage {
            next_token_id = TokenId 1
          , operators = [(FA2.OperatorKey
              { owner = bob
              , operator = alice
              , tokenId = TokenId 0
              }, ())]
          } }


    -- mint and burn work
  , nettestScenarioCaps "Mint burn" $ do
      setup <- doFA2Setup @("addresses" :# 2) @("tokens" :# 0)
      let alice ::< bob ::< SNil = sAddresses setup
      nft <- originateNft (exampleNftStorageWithAdmin alice)

      -- mint to bob
      let tokenMetadata0 = mkTokenMetadata "nft-symbol-0" "nft-name-0" "12"
      withSender alice $
        call nft (Call @"Mint") [MintTokenParam
          { token_metadata = FA2.TokenMetadata
              { tokenId = TokenId 0
              , tokenInfo = tokenMetadata0
              }
          , owner = bob
          }]

      -- alice is not an operator, so can't burn
      withSender alice $
        call nft (Call @"Burn") (TokenId 0, "nft-symbol-0")
          & expectError (unsafeMkMText "NOT_OPERATOR")

      -- bob makes alice an operator
      withSender bob $
        call nft (Call @"Update_operators") [AddOperator $ OperatorParam
          { opOwner = bob
          , opOperator = alice
          , opTokenId = TokenId 0
          }]

      -- bob's not an operator, so can't burn
      withSender bob $
        call nft (Call @"Burn") (TokenId 0, "nft-symbol-0")
          & expectError (unsafeMkMText "NOT_OPERATOR")

      -- alice is now an operator, so can burn
      withSender alice $
        call nft (Call @"Burn") (TokenId 0, "nft-symbol-0")

  ]

