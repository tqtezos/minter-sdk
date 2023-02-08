{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedLists #-}

-- | Tests for NFT multi-asset contract
module Test.MinterCollection.Nft where

import Prelude hiding (swap)

import Test.Tasty (TestTree, testGroup)

import Lorentz.Contracts.Spec.FA2Interface
import qualified Lorentz.Contracts.FA2 as FA2
import Lorentz.Value () -- ToAddress

import Michelson.Text (unsafeMkMText)
import Morley.Nettest
import Morley.Nettest.Tasty


import Lorentz.Contracts.MinterCollection.Nft.Contract (nftContract)
import Lorentz.Contracts.MinterCollection.Nft.Types

import Test.SimpleAdmin
import Test.Util


----------------------------------------------------------------------------------------
-- Originators
----------------------------------------------------------------------------------------

originateNft
  :: MonadNettest caps base m
  => NftStorage
  -> m (ContractHandler NftEntrypoints NftStorage)
originateNft storage =
  originateSimple "nft-multi-asset" storage nftContract

----------------------------------------------------------------------------------------
-- Test simple admin
----------------------------------------------------------------------------------------

-- Test SimpleAdmin admin ownership transfer
test_AdminChecks :: TestTree
test_AdminChecks =
  adminOwnershipTransferChecks @NftEntrypoints  @NftStorage
    (\admin ->
      originateNft
        (exampleNftStorageWithAdmin admin)
    )

----------------------------------------------------------------------------------------
-- Test data
----------------------------------------------------------------------------------------

tokenMetadata0 :: Show a => a -> TokenMetadata
tokenMetadata0 n = mkTokenMetadata ("nft-symbol-" <> show n) ("nft-name-" <> show n) "12"

tokenMetadata0' :: Natural -> FA2.TokenMetadata
tokenMetadata0' tokenId = FA2.TokenMetadata
  { tokenId = TokenId tokenId
  , tokenInfo = tokenMetadata0 $ tokenId
  }


----------------------------------------------------------------------------------------
-- Integrational tests
----------------------------------------------------------------------------------------

-- just transfer and ensure transferred
transferTest :: TestTree
transferTest = nettestScenarioCaps "Transfer" $ do
  setup <- doFA2Setup @("addresses" :# 4) @("tokens" :# 0)
  let admin ::< minter ::< alice ::< bob ::< SNil = sAddresses setup
  nft <- originateNft ((exampleNftStorageWithAdmin admin)
    { assets = exampleNftTokenStorage { ledger = [(TokenId 0, minter)] }
    })

  -- transfer from minter to alice, as admin, fails
  withSender admin $
    call nft (Call @"Transfer")
      [ TransferItem
        { tiFrom = minter
        , tiTxs = [ TransferDestination
            { tdTo = alice
            , tdTokenId = TokenId 0
            , tdAmount = 1
            } ]
        }
      ]
    & expectError (unsafeMkMText "FA2_NOT_OPERATOR")

  -- transfer from minter to alice
  withSender minter $
    call nft (Call @"Transfer")
      [ TransferItem
        { tiFrom = minter
        , tiTxs = [ TransferDestination
            { tdTo = alice
            , tdTokenId = TokenId 0
            , tdAmount = 1
            } ]
        }
      ]

  -- transfer from alice to bob
  withSender alice $
    call nft (Call @"Transfer")
      [ TransferItem
        { tiFrom = alice
        , tiTxs = [ TransferDestination
            { tdTo = bob
            , tdTokenId = TokenId 0
            , tdAmount = 1
            } ]
        }
      ]


-- just update metadata and ensure updated
updateMetadataTest :: TestTree
updateMetadataTest = nettestScenarioOnEmulatorCaps "Update metadata" $ do
  setup <- doFA2Setup @("addresses" :# 3) @("tokens" :# 0)
  let admin ::< minter ::< alice ::< SNil = sAddresses setup
  nft <- originateNft ((exampleNftStorageWithAdmin admin)
    { assets = exampleNftTokenStorage { ledger = [(TokenId 0, minter)]
                                      , next_token_id = TokenId 1
                                      } })

  -- alice can't update metadata, because not admin
  withSender alice $
    call nft (Call @"Update_metadata") [tokenMetadata0' 1]
      & expectError (unsafeMkMText "NOT_AN_ADMIN")

  -- admin can update metadata
  withSender admin $
    call nft (Call @"Update_metadata") [tokenMetadata0' 1]

  postUpdateStorage <- getStorage' nft
  postUpdateStorage @== (exampleNftStorageWithAdmin admin) {
    assets = exampleNftTokenStorage {
        ledger = [(TokenId 0, minter)]
      , next_token_id = TokenId 1
      , token_metadata = [(TokenId 1, tokenMetadata0' 1)]
      } }



-- just transfer using operator
operatorTest :: TestTree
operatorTest = nettestScenarioOnEmulatorCaps "Operator update and transfer" $ do
  setup <- doFA2Setup @("addresses" :# 4) @("tokens" :# 0)
  let admin ::< minter ::< alice ::< bob ::< SNil = sAddresses setup
  nft <- originateNft ((exampleNftStorageWithAdmin admin)
    { assets = exampleNftTokenStorage { ledger = [(TokenId 0, minter)] }
    })

  -- admin needs to set operator on (TokenId 0) to allow alice to mint
  withSender minter $
    call nft (Call @"Update_operators")
      [ AddOperator OperatorParam
          { opOwner = minter
          , opOperator = alice
          , opTokenId = TokenId 0
          }
      ]

  -- transfer from minter to bob, as admin, fails
  withSender admin $
    call nft (Call @"Transfer")
      [ TransferItem
        { tiFrom = minter
        , tiTxs = [ TransferDestination
            { tdTo = bob
            , tdTokenId = TokenId 0
            , tdAmount = 1
            } ]
        }
      ]
    & expectError (unsafeMkMText "FA2_NOT_OPERATOR")

  -- transfer from minter to bob, as alice (operator)
  withSender alice $
    call nft (Call @"Transfer")
      [ TransferItem
        { tiFrom = minter
        , tiTxs = [ TransferDestination
            { tdTo = bob
            , tdTokenId = TokenId 0
            , tdAmount = 1
            } ]
        }
      ]

  postTransferStorage <- getStorage' nft
  postTransferStorage @== (exampleNftStorageWithAdmin admin) {
    assets = exampleNftTokenStorage {
        ledger = [(TokenId 0, bob)]
      , next_token_id = TokenId 0
      , operators = [(FA2.OperatorKey
          { owner = minter
          , operator = alice
          , tokenId = TokenId 0
          }, ())]
      } }

  -- transfer from bob to minter
  withSender bob $
    call nft (Call @"Transfer")
      [ TransferItem
        { tiFrom = bob
        , tiTxs = [ TransferDestination
            { tdTo = minter
            , tdTokenId = TokenId 0
            , tdAmount = 1
            } ]
        }
      ]

  postTransferStorage2 <- getStorage' nft
  postTransferStorage2 @== (exampleNftStorageWithAdmin admin) {
    assets = exampleNftTokenStorage {
        ledger = [(TokenId 0, minter)]
      , next_token_id = TokenId 0
      , operators = [(FA2.OperatorKey
          { owner = minter
          , operator = alice
          , tokenId = TokenId 0
          }, ())]
      } }



-- just mint (holder of token_id=0 (#2) mints) (#1 is nft admin)
-- - mint using non-minter (#3) (fails)
-- - mint using minter (#2)
-- - ensure minted to expected target and can be transferred to user #3
mintTest :: TestTree
mintTest = nettestScenarioCaps "Mint" $ do
  setup <- doFA2Setup @("addresses" :# 5) @("tokens" :# 0)
  let admin ::< minter ::< alice ::< bob ::< charlie ::< SNil = sAddresses setup

  nft <- originateNft ((exampleNftStorageWithAdmin admin)
    { assets = exampleNftTokenStorage { ledger = [(TokenId 0, admin)]
                                      , next_token_id = TokenId 1
                                      } })

  -- admin can't mint because they're not an operator of token_id=0
  withSender admin $
    call nft (Call @"Mint") [MintTokenParam
      { token_metadata = tokenMetadata0' 1
      , owner = alice
      }]
      & expectError (unsafeMkMText "NOT_MINTER")

  -- alice can't mint because they're not an operator of token_id=0
  withSender alice $
    call nft (Call @"Mint") [MintTokenParam
      { token_metadata = tokenMetadata0' 1
      , owner = bob
      }]
      & expectError (unsafeMkMText "NOT_MINTER")

  -- minter needs to set operator on (TokenId 0) to allow alice to mint
  withSender admin $
    call nft (Call @"Update_operators")
      [ AddOperator OperatorParam
          { opOwner = admin
          , opOperator = minter
          , opTokenId = TokenId 0
          }
      ]

  -- minter can mint because they're an operator of token_id=0
  withSender minter $
    call nft (Call @"Mint") [MintTokenParam
      { token_metadata = tokenMetadata0' 1
      , owner = bob
      }]

  -- transfer from bob to charlie
  withSender bob $
    call nft (Call @"Transfer")
      [ TransferItem
        { tiFrom = bob
        , tiTxs = [ TransferDestination
            { tdTo = charlie
            , tdTokenId = TokenId 1
            , tdAmount = 1
            } ]
        }
      ]

  -- admin can disable minter from being an operator of token_id=0
  -- (preventing minter from minting)
  withSender admin $
    call nft (Call @"Update_operators")
      [ RemoveOperator OperatorParam
          { opOwner = admin
          , opOperator = minter
          , opTokenId = TokenId 0
          }
      ]

  -- minter can no longer mint
  withSender minter $
    call nft (Call @"Mint") [MintTokenParam
      { token_metadata = tokenMetadata0' 2 -- 3 ??
      , owner = charlie
      }]
      & expectError (unsafeMkMText "NOT_MINTER")


-- ensure multiple users can be minters
multipleMinterTest :: TestTree
multipleMinterTest = nettestScenarioCaps "Multiple minters" $ do
  setup <- doFA2Setup @("addresses" :# 6) @("tokens" :# 0)
  let admin ::< minter1 ::< minter2 ::< alice ::< bob ::< charlie ::< SNil = sAddresses setup
  nft <- originateNft ((exampleNftStorageWithAdmin admin)
    { assets = exampleNftTokenStorage { ledger = [(TokenId 0, admin)]
                                      , next_token_id = TokenId 1
                                      } })

  -- minter1 can't mint because they're not an operator of token_id=0
  withSender minter1 $
    call nft (Call @"Mint") [MintTokenParam
      { token_metadata = tokenMetadata0' 1
      , owner = bob
      }]
      & expectError (unsafeMkMText "NOT_MINTER")

  -- minter2 can't mint because they're not an operator of token_id=0
  withSender minter2 $
    call nft (Call @"Mint") [MintTokenParam
      { token_metadata = tokenMetadata0' 1
      , owner = bob
      }]
      & expectError (unsafeMkMText "NOT_MINTER")

  -- admin needs to set operator on (TokenId 0) to allow minter1 and minter2 to mint
  withSender admin $
    call nft (Call @"Update_operators")
      [ AddOperator OperatorParam
          { opOwner = admin
          , opOperator = minter1
          , opTokenId = TokenId 0
          }
      , AddOperator OperatorParam
          { opOwner = admin
          , opOperator = minter2
          , opTokenId = TokenId 0
          }
      ]

  -- minter1 can mint because they're an operator of token_id=0
  withSender minter1 $
    call nft (Call @"Mint") [MintTokenParam
      { token_metadata = tokenMetadata0' 1
      , owner = alice
      }]

  -- minter2 can mint because they're an operator of token_id=0
  withSender minter2 $
    call nft (Call @"Mint") [MintTokenParam
      { token_metadata = tokenMetadata0' 2
      , owner = bob
      }]

  -- transfer from alice to charlie
  withSender alice $
    call nft (Call @"Transfer")
      [ TransferItem
        { tiFrom = alice
        , tiTxs = [ TransferDestination
            { tdTo = charlie
            , tdTokenId = TokenId 1
            , tdAmount = 1
            } ]
        }
      ]

  -- transfer from bob to charlie
  withSender bob $
    call nft (Call @"Transfer")
      [ TransferItem
        { tiFrom = bob
        , tiTxs = [ TransferDestination
            { tdTo = charlie
            , tdTokenId = TokenId 2
            , tdAmount = 1
            } ]
        }
      ]




-- storage updates work
-- - Mint (admin)
-- - Update_metadata (admin)
-- - Update_operators (alice -> admin)
-- - Burn (admin)
-- (emulated for easy access to storage)
mintUpdateBurnStorageTest :: TestTree
mintUpdateBurnStorageTest = nettestScenarioOnEmulatorCaps "Mint update burn: storage" $ do
  setup <- doFA2Setup @("addresses" :# 4) @("tokens" :# 0)
  let admin ::< minter ::< alice ::< bob ::< SNil = sAddresses setup
  nft <- originateNft ((exampleNftStorageWithAdmin admin)
    { assets = exampleNftTokenStorage { ledger = [(TokenId 0, admin)]
                                      , next_token_id = TokenId 1
                                      } })

  -- admin can't mint because they're not an operator of token_id=0
  withSender admin $
    call nft (Call @"Mint") [MintTokenParam
      { token_metadata = tokenMetadata0' 1
      , owner = bob
      }]
      & expectError (unsafeMkMText "NOT_MINTER")

  -- admin needs to set operator on (TokenId 0) to allow alice to mint
  withSender admin $
    call nft (Call @"Update_operators")
      [ AddOperator OperatorParam
          { opOwner = admin
          , opOperator = minter
          , opTokenId = TokenId 0
          }
      ]

  -- now minter can mint to alice
  withSender minter $
    call nft (Call @"Mint") [MintTokenParam
      { token_metadata = tokenMetadata0' 1
      , owner = alice
      }]

  postMintStorage <- getStorage' nft
  postMintStorage @== (exampleNftStorageWithAdmin admin) {
    assets = exampleNftTokenStorage {
        ledger = [(TokenId 0, admin), (TokenId 1, alice)]
      , next_token_id = TokenId 2
      , operators = [(FA2.OperatorKey
          { owner = admin
          , operator = minter
          , tokenId = TokenId 0
          }, ())]
      , token_metadata = [(TokenId 1, tokenMetadata0' 1)]
      } }

  -- -- bob can't update metadata, because not admin
  withSender bob $
    call nft (Call @"Update_metadata") [tokenMetadata0' 0]
      & expectError (unsafeMkMText "NOT_AN_ADMIN")

  -- admin (as admin) can update metadata
  withSender admin $
    call nft (Call @"Update_metadata") [tokenMetadata0' 0]

  -- admin is not an operator, so can't burn
  withSender admin $
    call nft (Call @"Burn") (TokenId 1, alice)
      & expectError (unsafeMkMText "NOT_BURNER")

  postOperatorStorage <- getStorage' nft
  postOperatorStorage @== (exampleNftStorageWithAdmin admin) {
    assets = exampleNftTokenStorage {
        next_token_id = TokenId 2
      , ledger = [(TokenId 0, admin), (TokenId 1, alice)]
      , operators = [(FA2.OperatorKey
                      { owner = admin
                      , operator = minter
                      , tokenId = TokenId 0
                      }, ())
                    ]
      , token_metadata = [(TokenId 0, tokenMetadata0' 0), (TokenId 1, tokenMetadata0' 1)]
      } }

  withSender bob $
    call nft (Call @"Burn") (TokenId 1, alice)
      & expectError (unsafeMkMText "NOT_BURNER")

  -- admin is not an operator of token_id=0, so can't burn
  withSender admin $
    call nft (Call @"Burn") (TokenId 1, alice)
      & expectError (unsafeMkMText "NOT_BURNER")

  -- minter is an operator of token_id=0, so can burn
  withSender minter $
    call nft (Call @"Burn") (TokenId 1, alice)

  -- ensure token no longer in ledger
  postBurnStorage <- getStorage' nft
  postBurnStorage @== (exampleNftStorageWithAdmin admin) {
    assets = exampleNftTokenStorage {
        next_token_id = TokenId 2
      , ledger = [(TokenId 0, admin)]
      , operators = [(FA2.OperatorKey
                      { owner = admin
                      , operator = minter
                      , tokenId = TokenId 0
                      }, ())
                    ]
      , token_metadata = [(TokenId 0, tokenMetadata0' 0)]
      } }


-- mint and burn work
mintUpdateBurnTest :: TestTree
mintUpdateBurnTest = nettestScenarioCaps "Mint burn" $ do
  setup <- doFA2Setup @("addresses" :# 4) @("tokens" :# 0)
  let admin ::< minter ::< alice ::< bob ::< SNil = sAddresses setup
  nft <- originateNft ((exampleNftStorageWithAdmin admin)
    { assets = exampleNftTokenStorage {
          next_token_id = TokenId 1

        , ledger = [(TokenId 0, admin)] }
    })

  -- minter needs to be an operator of token_id=0 to mint
  withSender admin $
    call nft (Call @"Update_operators") [AddOperator $ OperatorParam
      { opOwner = admin
      , opOperator = minter
      , opTokenId = TokenId 0
      }]

  -- mint to bob
  withSender minter $
    call nft (Call @"Mint") [MintTokenParam
      { token_metadata = tokenMetadata0' 1
      , owner = bob
      }]

  -- alice is not an operator, so can't burn
  withSender alice $
    call nft (Call @"Burn") (TokenId 1, bob)
      & expectError (unsafeMkMText "NOT_BURNER")

  -- admin makes alice an operator of token_id=0
  withSender admin $
    call nft (Call @"Update_operators") [AddOperator $ OperatorParam
      { opOwner = admin
      , opOperator = alice
      , opTokenId = TokenId 0
      }]

  -- bob's not an operator, so can't burn
  withSender bob $
    call nft (Call @"Burn") (TokenId 1, bob)
      & expectError (unsafeMkMText "NOT_BURNER")

  -- alice is now an operator of token_id=0, so can burn
  withSender minter $
    call nft (Call @"Burn") (TokenId 1, bob)

  -- the token can no longer be transferred and fails with an error
  -- demonstrating it doesn't exist
  withSender bob $
    call nft (Call @"Transfer")
      [ TransferItem
        { tiFrom = bob
        , tiTxs = [ TransferDestination
            { tdTo = alice
            , tdTokenId = TokenId 1
            , tdAmount = 1
            } ]
        }
      ]
    & expectError (unsafeMkMText "FA2_TOKEN_UNDEFINED")


test_Integrational :: TestTree
test_Integrational = testGroup "Integrational"
  [ transferTest
  , updateMetadataTest
  , operatorTest
  , mintTest
  , mintUpdateBurnStorageTest
  , mintUpdateBurnTest
  ]

