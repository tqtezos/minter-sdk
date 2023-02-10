{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE InstanceSigs #-}

-- | Tests for bonding curve contract
module Test.BondingCurve where

import Fmt (Buildable)
import Prelude hiding (swap)
import System.IO (writeFile)

import qualified Data.Text.Lazy as L
import Test.Tasty (TestTree, testGroup)

import Lorentz.Base
import Lorentz.Value
import Michelson.Printer
import Michelson.Text (unsafeMkMText)
import Michelson.Typed.Scope (ConstantScope)
import Michelson.Typed.Sing () -- (KnownT)
import Morley.Nettest
import Morley.Nettest.Tasty
import Tezos.Address

import qualified Lorentz.Contracts.FA2 as FA2
import Lorentz.Contracts.Spec.FA2Interface
import Lorentz.Contracts.BondingCurve
import Lorentz.Contracts.BondingCurve.Interface
import Lorentz.Contracts.BondingCurve.Interface.Debug (DebugEntrypoints(..))
import Lorentz.Contracts.MinterCollection.Nft.Types

import Test.Util

import Test.SimpleAdmin
import Test.MinterCollection.Nft (originateNft)

----------------------------------------------------------------------------------------
-- Originators
----------------------------------------------------------------------------------------

originateBondingCurve
  :: MonadNettest caps base m
  => Storage (Lambda Natural Mutez)
  -> m (ContractHandler Entrypoints (Storage (Lambda Natural Mutez)))
originateBondingCurve storage =
  originateSimple "bonding-curve" storage bondingCurveContract

originateBondingCurveWithBalance
  :: MonadNettest caps base m
  => Mutez
  -> Storage (Lambda Natural Mutez)
  -> m (ContractHandler Entrypoints (Storage (Lambda Natural Mutez)))
originateBondingCurveWithBalance balance storage =
  originate $ OriginateData
    { odName = "bonding-curve"
    , odBalance = balance
    , odStorage = storage
    , odContract = bondingCurveContract
    }

originateDebugBondingCurve
  :: MonadNettest caps base m
  => Storage (Lambda Natural Mutez)
  -> m (ContractHandler DebugEntrypoints (Storage (Lambda Natural Mutez)))
originateDebugBondingCurve storage =
  originateSimple "debug-bonding-curve" storage debugBondingCurveContract

originateBondingCurvePiecewise
  :: MonadNettest caps base m
  => Storage PiecewisePolynomial
  -> m (ContractHandler Entrypoints (Storage PiecewisePolynomial))
originateBondingCurvePiecewise storage =
  originateSimple "bonding-curve-piecewise" storage bondingCurvePiecewiseContract

originateBondingCurvePiecewiseWithBalance
  :: MonadNettest caps base m
  => Mutez
  -> Storage PiecewisePolynomial
  -> m (ContractHandler Entrypoints (Storage PiecewisePolynomial))
originateBondingCurvePiecewiseWithBalance balance storage =
  originate $ OriginateData
    { odName = "bonding-curve-piecewise"
    , odBalance = balance
    , odStorage = storage
    , odContract = bondingCurvePiecewiseContract
    }

originateDebugBondingCurvePiecewise
  :: MonadNettest caps base m
  => Storage PiecewisePolynomial
  -> m (ContractHandler DebugEntrypoints (Storage PiecewisePolynomial))
originateDebugBondingCurvePiecewise storage =
  originateSimple "debug-bonding-curve-piecewise" storage debugBondingCurvePiecewiseContract

----------------------------------------------------------------------------------------
-- Admin tests
----------------------------------------------------------------------------------------

-- Test SimpleAdmin admin ownership transfer
test_AdminChecks :: TestTree
test_AdminChecks =
  adminOwnershipTransferChecks @Entrypoints  @(Storage (Lambda Natural Mutez))
    (\admin ->
      originateBondingCurve
        (exampleStorageWithAdmin admin)
    )

-- Test SimpleAdmin admin ownership transfer
test_AdminChecksPiecewise :: TestTree
test_AdminChecksPiecewise =
  adminOwnershipTransferChecks @Entrypoints  @(Storage PiecewisePolynomial)
    (\admin ->
      originateBondingCurvePiecewise
        (exampleStoragePiecewiseWithAdmin admin)
    )

----------------------------------------------------------------------------------------
-- Test data
----------------------------------------------------------------------------------------

tokenMetadata0 :: TokenMetadata
tokenMetadata0 = mkTokenMetadata "nft-symbol-0" "nft-name-0" "12"

tokenMetadata0' :: TokenId -> FA2.TokenMetadata
tokenMetadata0' tokenId = FA2.TokenMetadata
  { tokenId = tokenId
  , tokenInfo = tokenMetadata0
  }



----------------------------------------------------------------------------------------
-- Integration tests
----------------------------------------------------------------------------------------

withdrawTest :: forall c.
     String
  -> (forall caps base m. MonadNettest caps base m
      => Address
      -> Address
      -> Mutez
      -> m (ContractHandler Entrypoints (Storage c)))
  -> TestTree
withdrawTest name originator = nettestScenarioCaps ("Withdraw " <> name)  $ do
  setup <- doFA2Setup
  let admin ::< alice ::< SNil = sAddresses setup
  let !SNil = sTokens setup

  -- ensure admin has no tez
  withSender admin $
    getBalance admin >>= transferMoney alice
  getBalance admin @@== 0

  let withdrawAmount = 1234
  bondingCurve <- originator admin alice withdrawAmount

  -- admin only
  withSender alice $
    call bondingCurve (Call @"Withdraw") ()
      & expectError (unsafeMkMText "NOT_AN_ADMIN")

  withSender admin $
    call bondingCurve (Call @"Withdraw") ()

  getBalance admin @@== withdrawAmount

withdrawTestPiecewise :: TestTree
withdrawTestPiecewise = withdrawTest @PiecewisePolynomial "Piecewise" $ \admin alice withdrawAmount -> do
  let bondingCurveStorage :: Storage PiecewisePolynomial =
        (exampleStoragePiecewiseWithAdmin admin)
          {
            market_contract = alice
          , unclaimed = withdrawAmount
          }
  originateBondingCurvePiecewiseWithBalance withdrawAmount bondingCurveStorage

withdrawTestLambda :: TestTree
withdrawTestLambda = withdrawTest @(Lambda Natural Mutez) "Lambda" $ \admin alice withdrawAmount -> do
  let bondingCurveStorage :: Storage (Lambda Natural Mutez) =
        (exampleStorageWithAdmin admin)
          {
            market_contract = alice
          , unclaimed = withdrawAmount
          }
  originateBondingCurveWithBalance withdrawAmount bondingCurveStorage


withdrawBakingRewardsTest :: forall c. (Buildable c, Eq c)
  => String
  -> (forall caps base m. MonadNettest caps base m
      => Address
      -> Address
      -> m (Storage c, ContractHandler Entrypoints (Storage c)))
  -> TestTree
withdrawBakingRewardsTest name originator = nettestScenarioOnEmulatorCaps ("Withdraw Baking Rewards " <> name)  $ do
  setup <- doFA2Setup
  let admin ::< alice ::< bob ::< SNil = sAddresses setup
  let !SNil = sTokens setup
  halfAdminBalance <- (`div` 2) <$> getBalance admin

  -- ensure admin has no tez and that alice, bob have sufficient tez
  withSender admin $ do
    transferMoney alice halfAdminBalance
    transferMoney bob halfAdminBalance
  getBalance admin @@== 0

  let aliceRewardAmount = 12
  let bobRewardAmount = 24
  let withdrawAmount = aliceRewardAmount + bobRewardAmount
  (bondingCurveStorage, bondingCurve) <- originator admin alice

  withSender alice $
    transferMoney bondingCurve aliceRewardAmount

  postAliceRewardStorage <- getStorage' bondingCurve
  postAliceRewardStorage @== bondingCurveStorage { unclaimed = aliceRewardAmount }

  withSender bob $
    transfer $
      TransferData
        { tdTo = bondingCurve
        , tdAmount = bobRewardAmount
        , tdEntrypoint = DefEpName
        , tdParameter = ()
        }

  postBobRewardStorage <- getStorage' bondingCurve
  postBobRewardStorage @== bondingCurveStorage { unclaimed = withdrawAmount }

  -- admin only
  withSender alice $
    call bondingCurve (Call @"Withdraw") ()
      & expectError (unsafeMkMText "NOT_AN_ADMIN")

  withSender admin $
    call bondingCurve (Call @"Withdraw") ()

  getBalance admin @@== withdrawAmount

withdrawBakingRewardsTestPiecewise :: TestTree
withdrawBakingRewardsTestPiecewise = withdrawBakingRewardsTest @PiecewisePolynomial "Piecewise" $ \admin alice -> do
  let bondingCurveStorage :: Storage PiecewisePolynomial =
        (exampleStoragePiecewiseWithAdmin admin)
          {
            market_contract = alice
          , unclaimed = 0
          }
  bondingCurve <- originateBondingCurvePiecewise bondingCurveStorage
  return (bondingCurveStorage, bondingCurve)

withdrawBakingRewardsTestLambda :: TestTree
withdrawBakingRewardsTestLambda = withdrawBakingRewardsTest @(Lambda Natural Mutez) "Lambda" $ \admin alice -> do
  let bondingCurveStorage :: Storage (Lambda Natural Mutez) =
        (exampleStorageWithAdmin admin)
          {
            market_contract = alice
          , unclaimed = 0
          }
  bondingCurve <- originateBondingCurve bondingCurveStorage
  return (bondingCurveStorage, bondingCurve)


buyNoMintTest :: forall c.
     String
  -> (forall caps base m. MonadNettest caps base m
      => Address
      -> Address
      -> m (ContractHandler Entrypoints (Storage c)))
  -> TestTree
buyNoMintTest name originator = nettestScenarioCaps ("Buy: NO_MINT " <> name) $ do
  setup <- doFA2Setup
  let admin ::< alice ::< SNil = sAddresses setup
  let !SNil = sTokens setup

  bondingCurve <- originator admin alice

  withSender alice $
    call bondingCurve (Call @"Buy") ()
      & expectError (unsafeMkMText "NO_MINT")

buyNoMintTestPiecewise :: TestTree
buyNoMintTestPiecewise = buyNoMintTest @PiecewisePolynomial "Piecewise" $ \admin alice -> do
  let bondingCurveStorage :: Storage PiecewisePolynomial =
        (exampleStoragePiecewiseWithAdmin admin)
          {
            market_contract = alice
          , cost_mutez = constantPiecewisePolynomial 0
          }
  originateBondingCurvePiecewise bondingCurveStorage

buyNoMintTestLambda :: TestTree
buyNoMintTestLambda = buyNoMintTest @(Lambda Natural Mutez) "Lambda" $ \admin alice -> do
  let bondingCurveStorage :: Storage (Lambda Natural Mutez) =
        (exampleStorageWithAdmin admin)
          {
            market_contract = alice
          , cost_mutez = constantLambda 0
          }
  originateBondingCurve bondingCurveStorage


-- sell with token_index = 0 always fails with NO_TOKENS
sellTokenIndex0Test :: forall c.
     String
  -> (forall caps base m. MonadNettest caps base m
      => Address
      -> Address
      -> m (ContractHandler Entrypoints (Storage c)))
  -> TestTree
sellTokenIndex0Test name originator = nettestScenarioOnEmulatorCaps ("Sell: token_index = 0 " <> name) $ do
  setup <- doFA2Setup
  let admin ::< alice ::< SNil = sAddresses setup
  let !SNil = sTokens setup
  nft <- originateNft (exampleNftStorageWithAdmin admin)

  bondingCurve <- originator admin (toAddress nft)

  withSender admin $
    call bondingCurve (Call @"Sell") (TokenId 0)
      & expectError (unsafeMkMText "NO_TOKENS")

  withSender alice $
    call bondingCurve (Call @"Sell") (TokenId 0)
      & expectError (unsafeMkMText "NO_TOKENS")


sellTokenIndex0TestPiecewise :: TestTree
sellTokenIndex0TestPiecewise = sellTokenIndex0Test @PiecewisePolynomial "Piecewise" $ \admin nftAddress -> do
  let bondingCurveStorage :: Storage PiecewisePolynomial =
        (exampleStoragePiecewiseWithAdmin admin)
          {
            market_contract = nftAddress
          , token_index = 0
          }
  originateBondingCurvePiecewise bondingCurveStorage

sellTokenIndex0TestLambda :: TestTree
sellTokenIndex0TestLambda = sellTokenIndex0Test @(Lambda Natural Mutez) "Lambda" $ \admin nftAddress -> do
  let bondingCurveStorage :: Storage (Lambda Natural Mutez) =
        (exampleStorageWithAdmin admin)
          {
            market_contract = nftAddress
          , token_index = 0
          }
  originateBondingCurve bondingCurveStorage


-- sell with token_index = 0 always fails with NO_TOKENS
sellOffchainTokenIndex0Test :: forall c.
     String
  -> (forall caps base m. MonadNettest caps base m
      => Address
      -> Address
      -> m (ContractHandler Entrypoints (Storage c)))
  -> TestTree
sellOffchainTokenIndex0Test name originator = nettestScenarioOnEmulatorCaps ("Sell_offchain: token_index = 0 " <> name) $ do
  setup <- doFA2Setup
  let admin ::< alice ::< SNil = sAddresses setup
  let !SNil = sTokens setup
  nft <- originateNft (exampleNftStorageWithAdmin admin)
  bondingCurve <- originator admin (toAddress nft)

  withSender admin $
    call bondingCurve (Call @"Sell_offchain") (TokenId 0, admin)
      & expectError (unsafeMkMText "NO_TOKENS")

  withSender admin $
    call bondingCurve (Call @"Sell_offchain") (TokenId 0, alice)
      & expectError (unsafeMkMText "NO_TOKENS")

sellOffchainTokenIndex0TestPiecewise :: TestTree
sellOffchainTokenIndex0TestPiecewise = sellOffchainTokenIndex0Test @PiecewisePolynomial "Piecewise" $ \admin nftAddress -> do
  let bondingCurveStorage :: Storage PiecewisePolynomial =
        (exampleStoragePiecewiseWithAdmin admin)
          {
            market_contract = nftAddress
          , cost_mutez = constantPiecewisePolynomial 0
          , token_index = 0
          }
  originateBondingCurvePiecewise bondingCurveStorage

sellOffchainTokenIndex0TestLambda :: TestTree
sellOffchainTokenIndex0TestLambda = sellOffchainTokenIndex0Test @(Lambda Natural Mutez) "Lambda" $ \admin nftAddress -> do
  let bondingCurveStorage :: Storage (Lambda Natural Mutez) =
        (exampleStorageWithAdmin admin)
          {
            market_contract = nftAddress
          , cost_mutez = constantLambda 0
          , token_index = 0
          }
  originateBondingCurve bondingCurveStorage



--- too little/much tez
--- Spec:
--  + Mints token using `token_metadata` from storage to buyer
--  + Increments `token_index`
--  + Adds the `basis_points` fee to the `unclaimed` tez in storage
buyTest :: forall c. (Buildable c, Eq c)
  => String
  -> (forall caps base m. MonadNettest caps base m
      => Address
      -> Address
      -> m (Storage c, ContractHandler DebugEntrypoints (Storage c)))
  -> TestTree
buyTest name originator = nettestScenarioOnEmulatorCaps ("Buy " <> name) $ do
  setup <- doFA2Setup
  let admin ::< alice ::< SNil = sAddresses setup
  let !SNil = sTokens setup
  nft <- originateNft ((exampleNftStorageWithAdmin admin)
    { assets = exampleNftTokenStorage { ledger = [(TokenId 0, admin)]
                                      , next_token_id = TokenId 1
                                      } })
  (bondingCurveStorage, bondingCurve) <- originator admin (toAddress nft)

  -- admin needs to set operator on (TokenId 0) to allow bondingCurve to mint
  withSender admin $
    call nft (Call @"Update_operators")
      [ AddOperator OperatorParam
          { opOwner = admin
          , opOperator = toAddress bondingCurve
          , opTokenId = TokenId 0
          }
      ]

  withSender alice $
    call bondingCurve (Call @"Cost") 0
      & expectError (WrappedValue (0 :: Integer))

  preBuyStorage <- getStorage' bondingCurve
  preBuyStorage @== bondingCurveStorage

  withSender alice $
    call bondingCurve (Call @"Buy") ()
      & expectError (WrappedValue ((unsafeMkMText "WRONG_TEZ_PRICE", toEnum 0 :: Mutez), toEnum 10 :: Mutez))

  -- buy one token
  withSender alice $
    transfer $
      TransferData
        { tdTo = bondingCurve
        , tdAmount = 10
        , tdEntrypoint = ep "buy"
        , tdParameter = ()
        }

  postBuyStorage <- getStorage' bondingCurve
  postBuyStorage @== bondingCurveStorage { token_index = 1 }


buyTestPiecewise :: TestTree
buyTestPiecewise = buyTest @PiecewisePolynomial "Piecewise" $ \admin nftAddress -> do
  let bondingCurveStorage :: Storage PiecewisePolynomial =
        (exampleStoragePiecewiseWithAdmin admin)
          {
            market_contract = nftAddress
          , cost_mutez = constantPiecewisePolynomial 0
          , auction_price = 10
          }
  bondingCurve <- originateDebugBondingCurvePiecewise bondingCurveStorage
  return (bondingCurveStorage, bondingCurve)

buyTestLambda :: TestTree
buyTestLambda = buyTest @(Lambda Natural Mutez) "Lambda" $ \admin nftAddress -> do
  let bondingCurveStorage :: Storage (Lambda Natural Mutez) =
        (exampleStorageWithAdmin admin)
          {
            market_contract = nftAddress
          , cost_mutez = constantLambda 0
          , auction_price = 10
          }
  bondingCurve <- originateDebugBondingCurve bondingCurveStorage
  return (bondingCurveStorage, bondingCurve)


buyOffchainTest :: forall c. (Buildable c, Eq c)
  => String
  -> (forall caps base m. MonadNettest caps base m
      => Address
      -> Address
      -> m (Storage c, ContractHandler DebugEntrypoints (Storage c)))
  -> TestTree
buyOffchainTest name originator = nettestScenarioOnEmulatorCaps ("Buy_offchain " <> name) $ do
  setup <- doFA2Setup
  let admin ::< alice ::< bob ::< charlie ::< SNil = sAddresses setup
  let !SNil = sTokens setup
  nft <- originateNft ((exampleNftStorageWithAdmin admin)
    { assets = exampleNftTokenStorage { ledger = [(TokenId 0, admin)]
                                      , next_token_id = TokenId 1
                                      } })
  (bondingCurveStorage, bondingCurve) <- originator admin (toAddress nft)

  -- admin needs to set operator on (TokenId 0) to allow bondingCurve to mint
  withSender admin $
    call nft (Call @"Update_operators")
      [ AddOperator OperatorParam
          { opOwner = admin
          , opOperator = toAddress bondingCurve
          , opTokenId = TokenId 0
          }
      ]

  -- not admin only
  withSender alice $
    call bondingCurve (Call @"Buy_offchain") alice

  withSender admin $
    call bondingCurve (Call @"Buy_offchain") alice

  withSender bob $
    call bondingCurve (Call @"Buy_offchain") bob

  withSender admin $
    call bondingCurve (Call @"Buy") ()

  -- the token admin bought can't be transferred by charlie
  withSender charlie $
    call nft (Call @"Transfer")
      [ TransferItem
        { tiFrom = admin
        , tiTxs = [ TransferDestination
            { tdTo = charlie
            , tdTokenId = TokenId 4
            , tdAmount = 1
            } ]
        }
      ]
    & expectError (unsafeMkMText "FA2_NOT_OPERATOR")

  -- the token admin bought can be transferred by admin to charlie
  withSender admin $
    call nft (Call @"Transfer")
      [ TransferItem
        { tiFrom = admin
        , tiTxs = [ TransferDestination
            { tdTo = charlie
            , tdTokenId = TokenId 4
            , tdAmount = 1
            } ]
        }
      ]

  postBuyNftStorage <- getStorage' nft
  postBuyNftStorage @== (exampleNftStorageWithAdmin admin) {
    assets = exampleNftTokenStorage {
        next_token_id = TokenId 5
      , ledger =
          [ (TokenId 0, admin)
          , (TokenId 1, alice)
          , (TokenId 2, alice)
          , (TokenId 3, bob)
          , (TokenId 4, charlie)
          ]
      , operators = [(FA2.OperatorKey
            { owner = admin
            , operator = toAddress bondingCurve
            , tokenId = TokenId 0
            }, ())]
      , token_metadata =
          [ (TokenId 1, tokenMetadata0' (TokenId 1))
          , (TokenId 2, tokenMetadata0' (TokenId 2))
          , (TokenId 3, tokenMetadata0' (TokenId 3))
          , (TokenId 4, tokenMetadata0' (TokenId 4))
          ]
      } }

  postBuyStorage <- getStorage' bondingCurve
  postBuyStorage @== bondingCurveStorage { token_index = 4 }


buyOffchainTestPiecewise :: TestTree
buyOffchainTestPiecewise = buyOffchainTest @PiecewisePolynomial "Piecewise" $ \admin nftAddress -> do
  let bondingCurveStorage :: Storage PiecewisePolynomial =
        (exampleStoragePiecewiseWithAdmin admin)
          {
            market_contract = nftAddress
          , cost_mutez = constantPiecewisePolynomial 0
          , token_metadata = tokenMetadata0
          }
  bondingCurve <- originateDebugBondingCurvePiecewise bondingCurveStorage
  return (bondingCurveStorage, bondingCurve)

buyOffchainTestLambda :: TestTree
buyOffchainTestLambda = buyOffchainTest @(Lambda Natural Mutez) "Lambda" $ \admin nftAddress -> do
  let bondingCurveStorage :: Storage (Lambda Natural Mutez) =
        (exampleStorageWithAdmin admin)
          {
            market_contract = nftAddress
          , cost_mutez = constantLambda 0
          , token_metadata = tokenMetadata0
          }
  bondingCurve <- originateDebugBondingCurve bondingCurveStorage
  return (bondingCurveStorage, bondingCurve)



buyBatchOffchainTest :: forall c. (Buildable c, Eq c)
  => String
  -> (forall caps base m. MonadNettest caps base m
      => Address
      -> Address
      -> m (Storage c, ContractHandler DebugEntrypoints (Storage c)))
  -> TestTree
buyBatchOffchainTest name originator = nettestScenarioOnEmulatorCaps ("Buy_offchain (batch) " <> name) $ do
  setup <- doFA2Setup
  let admin ::< alice ::< bob ::< SNil = sAddresses setup
  let !SNil = sTokens setup
  nft <- originateNft ((exampleNftStorageWithAdmin admin)
    { assets = exampleNftTokenStorage { ledger = [(TokenId 0, admin)]
                                      , next_token_id = TokenId 1
                                      } })
  (bondingCurveStorage, bondingCurve) <- originator admin (toAddress nft)

  -- admin needs to set operator on (TokenId 0) to allow bondingCurve to mint
  withSender admin $
    call nft (Call @"Update_operators")
      [ AddOperator OperatorParam
          { opOwner = admin
          , opOperator = toAddress bondingCurve
          , opTokenId = TokenId 0
          }
      ]

  withSender admin $
    call bondingCurve (Call @"Buy_offchain") alice

  withSender admin $
    call bondingCurve (Call @"Buy_offchain") bob

  postBuyNftStorage <- getStorage' nft
  postBuyNftStorage @== (exampleNftStorageWithAdmin admin) {
    assets = exampleNftTokenStorage {
        next_token_id = TokenId 3
      , ledger = [(TokenId 0, admin), (TokenId 1, alice), (TokenId 2, bob)]
      , operators = [(FA2.OperatorKey
            { owner = admin
            , operator = toAddress bondingCurve
            , tokenId = TokenId 0
            }, ())]
      , token_metadata = [(TokenId 1, tokenMetadata0' (TokenId 1)), (TokenId 2, tokenMetadata0' (TokenId 2))]
      } }

  postBuyStorage <- getStorage' bondingCurve
  postBuyStorage @== bondingCurveStorage { token_index = 2 }


buyBatchOffchainTestPiecewise :: TestTree
buyBatchOffchainTestPiecewise = buyBatchOffchainTest @PiecewisePolynomial "Piecewise" $ \admin nftAddress -> do
  let bondingCurveStorage :: Storage PiecewisePolynomial =
        (exampleStoragePiecewiseWithAdmin admin)
          {
            market_contract = nftAddress
          , cost_mutez = constantPiecewisePolynomial 0
          , token_metadata = tokenMetadata0
          }
  bondingCurve <- originateDebugBondingCurvePiecewise bondingCurveStorage
  return (bondingCurveStorage, bondingCurve)

buyBatchOffchainTestLambda :: TestTree
buyBatchOffchainTestLambda = buyBatchOffchainTest @(Lambda Natural Mutez) "Lambda" $ \admin nftAddress -> do
  let bondingCurveStorage :: Storage (Lambda Natural Mutez) =
        (exampleStorageWithAdmin admin)
          {
            market_contract = nftAddress
          , cost_mutez = constantLambda 0
          , token_metadata = tokenMetadata0
          }
  bondingCurve <- originateDebugBondingCurve bondingCurveStorage
  return (bondingCurveStorage, bondingCurve)


--- call w/ admin (no tokens owned)
--- call w/ seller
--- Spec:
--  + Price is calculared as in `Buy`, without the `basis_points` fee:
--    * `auction_price`
--    * `cost_mutez` applied to `token_index`
--  + The token is burned on the FA2 marketplace
--  + Tez equal to the price is sent to the seller
-- , nettestScenarioCaps "Sell" $ do
sellTest :: forall c.
     String
  -> (forall caps base m. MonadNettest caps base m
      => Address
      -> Address
      -> m (ContractHandler Entrypoints (Storage c)))
  -> TestTree
sellTest name originator = nettestScenarioOnEmulatorCaps ("Sell " <> name) $ do
  setup <- doFA2Setup
  let admin ::< minter ::< alice ::< bob ::< SNil = sAddresses setup
  let !SNil = sTokens setup
  nft <- originateNft ((exampleNftStorageWithAdmin admin)
    { assets = exampleNftTokenStorage { ledger = [(TokenId 0, admin)]
                                      , next_token_id = TokenId 1
                                      } })
  bondingCurve <- originator admin (toAddress nft)

  -- admin needs to set operator on (TokenId 0) to allow bondingCurve to mint
  withSender admin $
    call nft (Call @"Update_operators")
      [ AddOperator OperatorParam
          { opOwner = admin
          , opOperator = toAddress bondingCurve
          , opTokenId = TokenId 0
          }
      , AddOperator OperatorParam
          { opOwner = admin
          , opOperator = minter
          , opTokenId = TokenId 0
          }
      ]

  -- alice can't sell a token that doesn't exist
  withSender alice $
    call bondingCurve (Call @"Sell") (TokenId 1)
      & expectError (unsafeMkMText "WRONG_ID")

  -- mint to alice
  withSender minter $
    call nft (Call @"Mint") [MintTokenParam
      { token_metadata = tokenMetadata0' (TokenId 1)
      , owner = alice
      }]

  -- bob can't sell alice's token
  withSender bob $
    call bondingCurve (Call @"Sell") (TokenId 1)
      & expectError (unsafeMkMText "NOT_BURNER")

  -- before token_id=1 burned
  preBurnStorage <- getStorage' nft
  preBurnStorage @== (exampleNftStorageWithAdmin admin) {
    assets = exampleNftTokenStorage {
        next_token_id = TokenId 2
      , ledger = [(TokenId 0, admin), (TokenId 1, alice)]
      , operators = [(FA2.OperatorKey
            { owner = admin
            , operator = toAddress bondingCurve
            , tokenId = TokenId 0
            }, ())
          , (FA2.OperatorKey
            { owner = admin
            , operator = minter
            , tokenId = TokenId 0
            }, ())
          ]
      , token_metadata = [(TokenId 1, tokenMetadata0' (TokenId 1))]
      } }

  withSender alice $
    call bondingCurve (Call @"Sell") (TokenId 1)

  -- can't sell twice
  withSender alice $
    call bondingCurve (Call @"Sell") (TokenId 1)
      & expectError (unsafeMkMText "NO_TOKENS")

  -- ensure tokenId0 burned
  postBurnStorage <- getStorage' nft
  postBurnStorage @== (exampleNftStorageWithAdmin admin) {
    assets = exampleNftTokenStorage {
        next_token_id = TokenId 2
      , ledger = [(TokenId 0, admin)]
      , operators = [(FA2.OperatorKey
            { owner = admin
            , operator = toAddress bondingCurve
            , tokenId = TokenId 0
            }, ())
          , (FA2.OperatorKey
            { owner = admin
            , operator = minter
            , tokenId = TokenId 0
            }, ())
          ]
      } }


sellTestPiecewise :: TestTree
sellTestPiecewise = sellTest @PiecewisePolynomial "Piecewise" $ \admin nftAddress -> do
  let bondingCurveStorage :: Storage PiecewisePolynomial =
        (exampleStoragePiecewiseWithAdmin admin)
          {
            market_contract = nftAddress
          , cost_mutez = constantPiecewisePolynomial 0
          , token_index = 1 -- token_index must be > 0 to sell
          , token_metadata = tokenMetadata0
          }
  originateBondingCurvePiecewise bondingCurveStorage

sellTestLambda :: TestTree
sellTestLambda = sellTest @(Lambda Natural Mutez) "Lambda" $ \admin nftAddress -> do
  let bondingCurveStorage :: Storage (Lambda Natural Mutez) =
        (exampleStorageWithAdmin admin)
          {
            market_contract = nftAddress
          , cost_mutez = constantLambda 0
          , token_index = 1 -- token_index must be > 0 to sell
          , token_metadata = tokenMetadata0
          }
  originateBondingCurve bondingCurveStorage


sellOffchainTest :: forall c.
     String
  -> (forall caps base m. MonadNettest caps base m
      => Mutez
      -> Address
      -> Address
      -> m (ContractHandler Entrypoints (Storage c)))
  -> TestTree
sellOffchainTest name originator = nettestScenarioOnEmulatorCaps ("Sell_offchain " <> name) $ do
  setup <- doFA2Setup
  let admin ::< minter ::< alice ::< bob ::< SNil = sAddresses setup
  let !SNil = sTokens setup
  nft <- originateNft ((exampleNftStorageWithAdmin admin)
    { assets = exampleNftTokenStorage { ledger = [(TokenId 0, admin)]
                                      , next_token_id = TokenId 1
                                      } })
  bondingCurve <- originator 10 admin (toAddress nft)

  -- admin needs to set operator on (TokenId 0) to allow bondingCurve to mint
  withSender admin $
    call nft (Call @"Update_operators")
      [ AddOperator OperatorParam
          { opOwner = admin
          , opOperator = toAddress bondingCurve
          , opTokenId = TokenId 0
          }
      , AddOperator OperatorParam
          { opOwner = admin
          , opOperator = minter
          , opTokenId = TokenId 0
          }
      ]

  -- admin only
  withSender alice $
    call bondingCurve (Call @"Sell_offchain") (TokenId 1, alice)
      & expectError (unsafeMkMText "NOT_AN_ADMIN")

  withSender admin $
    call bondingCurve (Call @"Sell_offchain") (TokenId 1, alice)
      & expectError (unsafeMkMText "WRONG_ID")

  -- mint to alice
  withSender minter $
    call nft (Call @"Mint") [MintTokenParam
      { token_metadata = tokenMetadata0' (TokenId 1)
      , owner = alice
      }]


  -- bob can't sell alice's token
  withSender bob $
    call bondingCurve (Call @"Sell") (TokenId 1)
      & expectError (unsafeMkMText "NOT_BURNER")

  preSellStorage <- getStorage' nft
  preSellStorage @== (exampleNftStorageWithAdmin admin) {
    assets = exampleNftTokenStorage {
        next_token_id = TokenId 2
      , ledger = [(TokenId 0, admin), (TokenId 1, alice)]
      , operators = [(FA2.OperatorKey
            { owner = admin
            , operator = toAddress bondingCurve
            , tokenId = TokenId 0
            }, ())
          , (FA2.OperatorKey
            { owner = admin
            , operator = minter
            , tokenId = TokenId 0
            }, ())
          ]
      , token_metadata = [(TokenId 1, tokenMetadata0' (TokenId 1))]
      } }

  aliceBalanceBefore <- getBalance alice

  withSender admin $
    call bondingCurve (Call @"Sell_offchain") (TokenId 1, alice)

  aliceBalanceAfter <- getBalance alice
  (aliceBalanceAfter - aliceBalanceBefore) @== 10

  -- can't sell twice
  withSender admin $
    call bondingCurve (Call @"Sell_offchain") (TokenId 1, alice)
      & expectError (unsafeMkMText "NO_TOKENS")

  -- ensure (TokenId 1) burned
  postBurnStorage <- getStorage' nft
  postBurnStorage @== (exampleNftStorageWithAdmin admin) {
    assets = exampleNftTokenStorage {
        next_token_id = TokenId 2
      , ledger = [(TokenId 0, admin)]
      , operators = [(FA2.OperatorKey
            { owner = admin
            , operator = toAddress bondingCurve
            , tokenId = TokenId 0
            }, ())
          , (FA2.OperatorKey
            { owner = admin
            , operator = minter
            , tokenId = TokenId 0
            }, ())
          ]
      } }


sellOffchainTestPiecewise :: TestTree
sellOffchainTestPiecewise = sellOffchainTest @PiecewisePolynomial "Piecewise" $ \bondingCurveBalance admin nftAddress -> do
  let bondingCurveStorage :: Storage PiecewisePolynomial =
        (exampleStoragePiecewiseWithAdmin admin)
          {
            market_contract = nftAddress
          , cost_mutez = constantPiecewisePolynomial 10
          , token_index = 1 -- token_index > 0 to sell tokens, otherwise no tokens to sell
          , token_metadata = tokenMetadata0
          }
  originateBondingCurvePiecewiseWithBalance bondingCurveBalance bondingCurveStorage

sellOffchainTestLambda :: TestTree
sellOffchainTestLambda = sellOffchainTest @(Lambda Natural Mutez) "Lambda" $ \bondingCurveBalance admin nftAddress -> do
  let bondingCurveStorage :: Storage (Lambda Natural Mutez) =
        (exampleStorageWithAdmin admin)
          {
            market_contract = nftAddress
          , cost_mutez = constantLambda 10
          , token_index = 1 -- token_index must be > 0 to sell
          , token_metadata = tokenMetadata0
          }
  originateBondingCurveWithBalance bondingCurveBalance bondingCurveStorage


buySellTest :: forall c. (Buildable c, Eq c, ConstantScope (ToT c), IsoValue c)
  => String
  -> (forall caps base m.  MonadNettest caps base m
        => Mutez
        -> Natural
        -> Address
        -> Address
        -> m (Storage c, ContractHandler DebugEntrypoints (Storage c)))
  -> TestTree
buySellTest name originator = nettestScenarioOnEmulatorCaps ("Buy Sell " <> name) $ do
  let logFile = "buy_sell_test_data_" <> name <> ".txt"
  liftIO $ writeFile logFile "Buy Sell Test\n"

  let dontForceSingleLine = False
  let log = liftIO . appendFile logFile . ("\n" <>)

  setup <- doFA2Setup
  let admin ::< alice ::< bob ::< charlie ::< SNil = sAddresses setup

  log "(admin, alice, bob, charlie)"
  log $ show (formatAddress admin, formatAddress alice, formatAddress bob, formatAddress charlie)
  log ""

  let !SNil = sTokens setup
  let nftStorage = ((exampleNftStorageWithAdmin admin)
        { assets = exampleNftTokenStorage { ledger = [(TokenId 0, admin)]
                                          , next_token_id = TokenId 1
                                          } })
  log "nft storage"
  log . L.toStrict . printTypedValue dontForceSingleLine $ toVal nftStorage
  log ""

  nft <- originateNft nftStorage
  log "nft address"
  log . formatAddress $ toAddress nft
  log ""

  let auctionPrice = 100
  let basisPoints = 100
  (bondingCurveStorage, bondingCurve) <- originator auctionPrice basisPoints admin (toAddress nft)

  log "bonding curve storage"
  log . L.toStrict . printTypedValue dontForceSingleLine $ toVal bondingCurveStorage
  log ""

  log "bonding curve address"
  log . formatAddress $ toAddress bondingCurve
  log ""

  -- admin needs to set operator on (TokenId 0) to allow bondingCurve to mint
  let updateOperators :: [UpdateOperator] =
        [ AddOperator OperatorParam
            { opOwner = admin
            , opOperator = toAddress bondingCurve
            , opTokenId = TokenId 0
            }
        ]
  log "admin -> nft: update_operators"
  log . L.toStrict . printTypedValue dontForceSingleLine $ toVal updateOperators
  log ""
  withSender admin $
    call nft (Call @"Update_operators") updateOperators

  let buyers :: [(Integer, Address)] =
        [ (10, alice)
        , (60, bob)
        , (170, charlie)
        ]

  forM_ (zip [0..] buyers) $ \(index, (amount, buyer)) -> do

    withSender buyer $
      call bondingCurve (Call @"Cost") index
        & expectError (WrappedValue amount)

    let insufficientAmount :: Mutez = fromIntegral $ amount
    let buyAmount :: Mutez = fromIntegral $ fromIntegral auctionPrice + amount

    -- basis_points fee required
    withSender buyer $
      transfer (
        TransferData
          { tdTo = bondingCurve
          , tdAmount = insufficientAmount
          , tdEntrypoint = ep "buy"
          , tdParameter = ()
          })
        & expectError (WrappedValue ((unsafeMkMText "WRONG_TEZ_PRICE", insufficientAmount), buyAmount))

    log "buyer -> bondingCurve: buy"
    log "buyer:"
    log $ formatAddress buyer
    log "amount:"
    log . L.toStrict . printTypedValue dontForceSingleLine $ toVal buyAmount
    log ""
    withSender buyer $
      transfer $
        TransferData
          { tdTo = bondingCurve
          , tdAmount = buyAmount
          , tdEntrypoint = ep "buy"
          , tdParameter = ()
          }

  let sellers :: [(Natural, (Integer, Address))] = zip [1..] buyers

  forM_ (reverse sellers) $ \(tokenId, (expectedCost, seller)) -> do
    sellerBalanceBefore <- getBalance seller

    log "seller -> bondingCurve: sell"
    log "seller:"
    log $ formatAddress seller
    log "parameter:"
    log . L.toStrict . printTypedValue dontForceSingleLine $ toVal (TokenId tokenId)
    log ""
    withSender seller $
      call bondingCurve (Call @"Sell") (TokenId tokenId)

    let preFeeSellAmount = auctionPrice + fromIntegral expectedCost
    let sellAmount = fromInteger . removeBasisPointFee basisPoints . fromIntegral $ preFeeSellAmount

    -- ensure cost was expected
    sellerBalanceAfter <- getBalance seller
    (tokenId, (sellerBalanceAfter - sellerBalanceBefore)) @== (tokenId, sellAmount)

  -- ensure zero tokens remaining and unclaimed is expected
  postSellStorage <- getStorage' bondingCurve
  postSellStorage @== bondingCurveStorage { unclaimed = 4 }

  log "admin -> bondingCurve: withdraw"
  log "admin:"
  log $ formatAddress admin
  log "parameter:"
  log . L.toStrict . printTypedValue dontForceSingleLine $ toVal ()
  log ""
  withSender admin $
    call bondingCurve (Call @"Withdraw") ()

  getBalance bondingCurve @@== 0

  -- ensure storage reset after all tokens sold and Withdraw is called
  postWithdrawStorage <- getStorage' bondingCurve
  postWithdrawStorage @== bondingCurveStorage


buySellTestPiecewise :: TestTree
buySellTestPiecewise = buySellTest @PiecewisePolynomial "Piecewise" $ \auctionPrice basisPoints admin nftAddress -> do
  let bondingCurveStorage :: Storage PiecewisePolynomial =
        (exampleStoragePiecewiseWithAdmin admin)
          {
            market_contract = nftAddress
          , cost_mutez = polynomialToPiecewisePolynomial [10, 20, 30]
          , auction_price = auctionPrice
          , basis_points = basisPoints
          }
  bondingCurve <- originateDebugBondingCurvePiecewise bondingCurveStorage
  return (bondingCurveStorage, bondingCurve)

buySellTestLambda :: TestTree
buySellTestLambda = buySellTest @(Lambda Natural Mutez) "Lambda" $ \auctionPrice basisPoints admin nftAddress -> do
  let bondingCurveStorage :: Storage (Lambda Natural Mutez) =
        (exampleStorageWithAdmin admin)
          {
            market_contract = nftAddress
          , cost_mutez = constantsLambda $ fromInteger . runPiecewisePolynomial (polynomialToPiecewisePolynomial [10, 20, 30]) <$> [0..5]
          , auction_price = auctionPrice
          , basis_points = basisPoints
          }
  bondingCurve <- originateDebugBondingCurve bondingCurveStorage
  return (bondingCurveStorage, bondingCurve)



buySellOffchainTest :: forall c. (Buildable c, Eq c, ConstantScope (ToT c))
  => String
  -> (forall caps base m.  MonadNettest caps base m
        => Mutez
        -> Natural
        -> Address
        -> Address
        -> m (Storage c, ContractHandler DebugEntrypoints (Storage c)))
  -> TestTree
buySellOffchainTest name originator = nettestScenarioOnEmulatorCaps ("Buy Sell Offchain " <> name) $ do
  setup <- doFA2Setup
  let admin ::< alice ::< bob ::< charlie ::< SNil = sAddresses setup
  let !SNil = sTokens setup
  nft <- originateNft ((exampleNftStorageWithAdmin admin)
    { assets = exampleNftTokenStorage { ledger = [(TokenId 0, admin)]
                                      , next_token_id = TokenId 1
                                      } })

  let auctionPrice = 100
  let basisPoints = 100
  (bondingCurveStorage, bondingCurve) <- originator auctionPrice basisPoints admin (toAddress nft)

  -- admin needs to set operator on (TokenId 0) to allow bondingCurve to mint
  withSender admin $
    call nft (Call @"Update_operators")
      [ AddOperator OperatorParam
          { opOwner = admin
          , opOperator = toAddress bondingCurve
          , opTokenId = TokenId 0
          }
      ]

  let buyers :: [(Integer, Address)] =
        [ (10, alice)
        , (60, bob)
        , (170, charlie)
        ]

  forM_ (zip [0..] buyers) $ \(index, (amount, buyer)) -> do

    withSender buyer $
      call bondingCurve (Call @"Cost") index
        & expectError (WrappedValue amount)

    let insufficientAmount :: Mutez = fromIntegral amount
    let buyAmount :: Mutez = fromIntegral $ fromIntegral auctionPrice + amount

    -- basis_points fee required
    withSender admin $
      transfer (
        TransferData
          { tdTo = bondingCurve
          , tdAmount = insufficientAmount
          , tdEntrypoint = ep "buy_offchain"
          , tdParameter = buyer
          })
        & expectError (WrappedValue ((unsafeMkMText "WRONG_TEZ_PRICE", insufficientAmount), buyAmount))

    withSender admin $
      transfer $
        TransferData
          { tdTo = bondingCurve
          , tdAmount = buyAmount
          , tdEntrypoint = ep "buy_offchain"
          , tdParameter = buyer
          }

  let sellers :: [(Natural, (Integer, Address))] = zip [1..] buyers

  forM_ (reverse sellers) $ \(tokenId, (expectedCost, seller)) -> do
    sellerBalanceBefore <- getBalance seller

    withSender admin $
      call bondingCurve (Call @"Sell_offchain") (TokenId tokenId, seller)

    let sellAmount = fromInteger . removeBasisPointFee basisPoints . fromIntegral $ auctionPrice + fromIntegral expectedCost

    -- ensure cost was expected
    sellerBalanceAfter <- getBalance seller
    (tokenId, (sellerBalanceAfter - sellerBalanceBefore)) @== (tokenId, sellAmount)

  -- ensure zero tokens remaining and unclaimed is expected
  postSellStorage <- getStorage' bondingCurve
  postSellStorage @== bondingCurveStorage { unclaimed = 4 }

  withSender admin $
    call bondingCurve (Call @"Withdraw") ()

  getBalance bondingCurve @@== 0

  -- ensure storage reset after all tokens sold and Withdraw is called
  postWithdrawStorage <- getStorage' bondingCurve
  postWithdrawStorage @== bondingCurveStorage


buySellOffchainTestPiecewise :: TestTree
buySellOffchainTestPiecewise = buySellOffchainTest @PiecewisePolynomial "Piecewise" $ \auctionPrice basisPoints admin nftAddress -> do
  let bondingCurveStorage :: Storage PiecewisePolynomial =
        (exampleStoragePiecewiseWithAdmin admin)
          {
            market_contract = nftAddress
          , cost_mutez = polynomialToPiecewisePolynomial [10, 20, 30]
          , auction_price = auctionPrice
          , basis_points = basisPoints
          }
  bondingCurve <- originateDebugBondingCurvePiecewise bondingCurveStorage
  return (bondingCurveStorage, bondingCurve)

buySellOffchainTestLambda :: TestTree
buySellOffchainTestLambda = buySellOffchainTest @(Lambda Natural Mutez) "Lambda" $ \auctionPrice basisPoints admin nftAddress -> do
  let bondingCurveStorage :: Storage (Lambda Natural Mutez) =
        (exampleStorageWithAdmin admin)
          {
            market_contract = nftAddress
          , cost_mutez = constantsLambda $ fromInteger . runPiecewisePolynomial (polynomialToPiecewisePolynomial [10, 20, 30]) <$> [0..5]
          , auction_price = auctionPrice
          , basis_points = basisPoints
          }
  bondingCurve <- originateDebugBondingCurve bondingCurveStorage
  return (bondingCurveStorage, bondingCurve)



test_Integrational :: TestTree
test_Integrational = testGroup "Integrational"
  [ withdrawTestPiecewise
  , withdrawTestLambda

  , withdrawBakingRewardsTestPiecewise
  , withdrawBakingRewardsTestLambda

  , buyNoMintTestPiecewise
  , buyNoMintTestLambda

  , buyTestPiecewise
  , buyTestLambda

  , buyOffchainTestPiecewise
  , buyOffchainTestLambda

  , buyBatchOffchainTestPiecewise
  , buyBatchOffchainTestLambda

  , sellTokenIndex0TestPiecewise
  , sellTokenIndex0TestLambda

  , sellTestPiecewise
  , sellTestLambda

  , sellOffchainTokenIndex0TestPiecewise
  , sellOffchainTokenIndex0TestLambda

  , sellOffchainTestPiecewise
  , sellOffchainTestLambda

  , buySellTestPiecewise
  , buySellTestLambda

  , buySellOffchainTestPiecewise
  , buySellOffchainTestLambda
  ]

-- input, expectedOutput, storageF
--
-- storageF is applied to the generated admin address
callCostTest ::
     Natural
  -> Integer
  -> (Address -> Storage (Lambda Natural Mutez))
  -> TestTree
callCostTest input expectedOutput storageF =
  nettestScenarioCaps ("Call Lambda Cost with " ++ show input) $ do
    setup <- doFA2Setup @("addresses" :# 1) @("tokens" :# 0)
    let admin ::< SNil = sAddresses setup
    let bondingCurveStorage = storageF admin
    bondingCurve <- originateDebugBondingCurve bondingCurveStorage

    call bondingCurve (Call @"Cost") input
      & expectError (WrappedValue expectedOutput)

-- input, expectedOutput, storageF
--
-- storageF is applied to the generated admin address
callCostTestPiecewise ::
     Natural
  -> Integer
  -> (Address -> Storage PiecewisePolynomial)
  -> TestTree
callCostTestPiecewise input expectedOutput storageF =
  nettestScenarioCaps ("Call Piecewise Polynomial Cost with " ++ show input) $ do
    setup <- doFA2Setup @("addresses" :# 1) @("tokens" :# 0)
    let admin ::< SNil = sAddresses setup
    let bondingCurveStorage = storageF admin
    bondingCurve <- originateDebugBondingCurvePiecewise bondingCurveStorage

    call bondingCurve (Call @"Cost") input
      & expectError (WrappedValue expectedOutput)

-- input, expectedOutput, storageF
--
-- storageF is applied to the generated admin address
callPowTest ::
     Natural
  -> Natural
  -> Integer
  -> (Address -> Storage PiecewisePolynomial)
  -> TestTree
callPowTest x n expectedOutput storageF =
  nettestScenarioCaps ("Call Pow with " ++ show (x, n)) $ do
    setup <- doFA2Setup @("addresses" :# 1) @("tokens" :# 0)
    let admin ::< SNil = sAddresses setup
    let bondingCurveStorage = storageF admin
    bondingCurve <- originateDebugBondingCurvePiecewise bondingCurveStorage

    call bondingCurve (Call @"Pow") (x, n)
      & expectError (WrappedValue expectedOutput)


-- test cost function using the debug version of the contract
test_Debug :: TestTree
test_Debug = testGroup "Debug"
  [ -- default storage cost_mutez(4) == 34
    callCostTestPiecewise 4 39 exampleStoragePiecewiseWithAdmin

  -- (constantPiecewisePolynomial 0) cost_mutez(12) == 0
  , callCostTestPiecewise 12 0 (\admin -> (exampleStoragePiecewiseWithAdmin admin)
      { cost_mutez = constantPiecewisePolynomial 0 })

  , callPowTest 1 3 1 exampleStoragePiecewiseWithAdmin
  , callPowTest 2 3 8 exampleStoragePiecewiseWithAdmin
  , callPowTest 3 4 81 exampleStoragePiecewiseWithAdmin
  , callPowTest 2 10 1024 exampleStoragePiecewiseWithAdmin

  ]

