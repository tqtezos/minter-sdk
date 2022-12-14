{-# LANGUAGE OverloadedLists #-}

{-# LANGUAGE InstanceSigs #-}

-- | Tests for bonding curve contract
module Test.BondingCurve where

import Prelude hiding (swap)

-- import Hedgehog ((===), Gen, Property, forAll, property)
-- import qualified Hedgehog.Gen as Gen
-- import qualified Hedgehog.Range as Range
-- import qualified Data.Map as Map
import Test.Tasty (TestTree, testGroup)

-- import Lorentz.Errors
import Lorentz.Value
import Michelson.Interpret (MorleyLogs(..))
import Michelson.Text (unsafeMkMText)
import Michelson.Typed.Scope () -- (ConstantScope)
import Michelson.Typed.Sing () -- (KnownT)
import Morley.Nettest
import Morley.Nettest.Tasty
-- import Michelson.Runtime.GState (GState(..), asBalance)
-- import Michelson.Test.Integrational (InternalState(..))
-- import Morley.Nettest.Pure

import qualified Lorentz.Contracts.FA2 as FA2 -- (TokenMetadata(..))
import Lorentz.Contracts.Spec.FA2Interface
import Lorentz.Contracts.BondingCurve
import Lorentz.Contracts.BondingCurve.Interface
import Lorentz.Contracts.BondingCurve.Interface.Debug (DebugEntrypoints(..))
import Lorentz.Contracts.MinterCollection.Nft.Types
-- import Lorentz.Contracts.SimpleAdmin

-- import Test.Swaps.Util
import Test.Util

import Test.SimpleAdmin
import Test.MinterCollection.Nft (originateNft)

----------------------------------------------------------------------------------------
-- Originators
----------------------------------------------------------------------------------------

originateBondingCurve
  :: MonadNettest caps base m
  => Storage
  -> m (ContractHandler Entrypoints Storage)
originateBondingCurve storage =
  originateSimple "bonding-curve" storage bondingCurveContract

originateBondingCurveWithBalance
  :: MonadNettest caps base m
  => Mutez
  -> Storage
  -> m (ContractHandler Entrypoints Storage)
originateBondingCurveWithBalance balance storage =
  originate $ OriginateData
    { odName = "bonding-curve"
    , odBalance = balance
    , odStorage = storage
    , odContract = bondingCurveContract
    }

originateDebugBondingCurve
  :: MonadNettest caps base m
  => Storage
  -> m (ContractHandler DebugEntrypoints Storage)
originateDebugBondingCurve storage =
  originateSimple "debug-bonding-curve" storage debugBondingCurveContract


----------------------------------------------------------------------------------------
-- Admin tests
----------------------------------------------------------------------------------------

-- TODO: re-enable
-- -- Test SimpleAdmin admin ownership transfer
-- test_AdminChecks :: TestTree
-- test_AdminChecks =
--   adminOwnershipTransferChecks @Entrypoints  @Storage
--     (\admin ->
--       originateBondingCurve
--         (exampleStorageWithAdmin admin)
--     )


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

-- TODO: morley seems unable to test this with its emulator's current version
-- nettestScenarioCaps "Set_delegate" $ do
setDelegateTest :: TestTree
setDelegateTest = nettestScenarioOnEmulatorCaps "Set_delegate" $ do
  setup <- doFA2Setup
  let admin ::< alice ::< SNil = sAddresses setup
  let !SNil = sTokens setup
  let bondingCurveStorage :: Storage = exampleStorageWithAdmin admin
  bondingCurve <- originateBondingCurve bondingCurveStorage

  -- admin only
  withSender alice $
    call bondingCurve (Call @"Set_delegate") Nothing
      & expectError (unsafeMkMText "NOT_AN_ADMIN")

  withSender admin $
    call bondingCurve (Call @"Set_delegate") Nothing

  -- TODO ensure delegate set
  logs <- getMorleyLogs
  logs @== [MorleyLogs []]


withdrawTest :: TestTree
withdrawTest = nettestScenarioCaps "Withdraw" $ do
  setup <- doFA2Setup
  let admin ::< alice ::< SNil = sAddresses setup
  let !SNil = sTokens setup

  -- ensure admin has no tez
  withSender admin $
    getBalance admin >>= transferMoney alice
  getBalance admin @@== 0

  -- nft <- originateNft (exampleNftStorageWithAdmin alice)
  let withdrawAmount = 1234
  let bondingCurveStorage :: Storage =
        (exampleStorageWithAdmin admin)
          {
            market_contract = alice -- toAddress nft
          , unclaimed = withdrawAmount
          }
  bondingCurve <- originateBondingCurveWithBalance withdrawAmount bondingCurveStorage

  -- admin only
  withSender alice $
    call bondingCurve (Call @"Withdraw") ()
      & expectError (unsafeMkMText "NOT_AN_ADMIN")

  withSender admin $
    call bondingCurve (Call @"Withdraw") ()

  getBalance admin @@== withdrawAmount


buyNoMint :: TestTree
buyNoMint = nettestScenarioCaps "Buy: NO_MINT" $ do
  setup <- doFA2Setup
  let admin ::< alice ::< SNil = sAddresses setup
  let !SNil = sTokens setup
  let bondingCurveStorage :: Storage =
        (exampleStorageWithAdmin admin)
          {
            market_contract = alice
          , cost_mutez = constantPiecewisePolynomial 0
          }
  bondingCurve <- originateBondingCurve bondingCurveStorage

  withSender alice $
    call bondingCurve (Call @"Buy") ()
      & expectError (unsafeMkMText "NO_MINT")


--- too little/much tez
--- Spec:
--  + Mints token using `token_metadata` from storage to buyer
--  + Increments `token_index`
--  + Adds the `basis_points` fee to the `unclaimed` tez in storage
buyTest :: TestTree
buyTest = nettestScenarioCaps "Buy" $ do
  setup <- doFA2Setup
  let admin ::< alice ::< SNil = sAddresses setup
  let !SNil = sTokens setup
  nft <- originateNft (exampleNftStorageWithAdmin admin)
  let bondingCurveStorage :: Storage =
        (exampleStorageWithAdmin admin)
          {
            market_contract = toAddress nft
          , cost_mutez = constantPiecewisePolynomial 0
          }
  bondingCurve <- originateBondingCurve bondingCurveStorage

  withSender alice $
    call bondingCurve (Call @"Buy") ()
      & expectError (unsafeMkMText "WRONG_TEZ_PRICE")

  -- TODO: successful buy: which price?
  -- TODO: assert changes


-- TODO: buy-offchain
buyOffchainTest :: TestTree
buyOffchainTest = nettestScenarioCaps "Buy_offchain" $ do
  setup <- doFA2Setup
  let admin ::< alice ::< bob ::< SNil = sAddresses setup
  let !SNil = sTokens setup
  nft <- originateNft (exampleNftStorageWithAdmin admin)

  let bondingCurveStorage :: Storage =
        (exampleStorageWithAdmin admin)
          {
            market_contract = toAddress nft
          , cost_mutez = constantPiecewisePolynomial 0
          }
  bondingCurve <- originateBondingCurve bondingCurveStorage

  -- admin only
  withSender alice $
    call bondingCurve (Call @"Buy_offchain") alice
      & expectError (unsafeMkMText "NOT_AN_ADMIN")

  withSender admin $
    call bondingCurve (Call @"Buy_offchain") bob
      & expectError (unsafeMkMText "NOT_AN_ADMIN") -- TODO correct error ??

  withSender admin $
    call bondingCurve (Call @"Buy_offchain") alice
      & expectError (unsafeMkMText "WRONG_TEZ_PRICE")

  -- TODO: assert changes


-- sell with token_index = 0 always fails with NO_TOKENS
sellTokenIndex0 :: TestTree
sellTokenIndex0 = nettestScenarioOnEmulatorCaps "Sell: token_index = 0" $ do
  setup <- doFA2Setup
  let admin ::< alice ::< SNil = sAddresses setup
  let tokenId0 ::< SNil = sTokens setup
  nft <- originateNft (exampleNftStorageWithAdmin admin)
  let bondingCurveStorage :: Storage =
        (exampleStorageWithAdmin admin)
          {
            market_contract = toAddress nft
          , token_index = 0
          }
  bondingCurve <- originateBondingCurve bondingCurveStorage

  -- mint to alice
  withSender admin $
    call nft (Call @"Mint") [MintTokenParam
      { token_metadata = tokenMetadata0' tokenId0
      , owner = alice
      }]

  withSender alice $
    call bondingCurve (Call @"Sell") tokenId0
      & expectError (unsafeMkMText "NO_TOKENS")


-- TODO: sell
--- call w/ admin (no tokens owned)
--- call w/ seller
--- Spec:
--  + Price is calculared as in `Buy`, without the `basis_points` fee:
--    * `auction_price`
--    * `cost_mutez` applied to `token_index`
--  + The token is burned on the FA2 marketplace
--  + Tez equal to the price is sent to the seller
-- , nettestScenarioCaps "Sell" $ do
sellTest :: TestTree
sellTest = nettestScenarioOnEmulatorCaps "Sell" $ do
  setup <- doFA2Setup
  let admin ::< alice ::< bob ::< SNil = sAddresses setup
  let tokenId0 ::< SNil = sTokens setup
  nft <- originateNft (exampleNftStorageWithAdmin admin)

  let bondingCurveStorage :: Storage =
        (exampleStorageWithAdmin admin)
          {
            market_contract = toAddress nft
          , cost_mutez = constantPiecewisePolynomial 0
          , token_index = 1 -- token_index must be > 0 to sell
          }
  bondingCurve <- originateBondingCurve bondingCurveStorage

  -- alice can't sell a token that doesn't exist
  withSender alice $
    call bondingCurve (Call @"Sell") tokenId0
      & expectError (unsafeMkMText "WRONG_ID")

  -- mint to alice
  withSender admin $
    call nft (Call @"Mint") [MintTokenParam
      { token_metadata = tokenMetadata0' tokenId0
      , owner = alice
      }]

  -- bob can't sell alice's token
  withSender bob $
    call bondingCurve (Call @"Sell") tokenId0
      & expectError (unsafeMkMText "WRONG_SYMBOL")

  -- no operator set
  withSender alice $
    call bondingCurve (Call @"Sell") tokenId0
      & expectError (unsafeMkMText "WRONG_SYMBOL")

  -- alice needs to set operator to sell
  withSender alice $
    call nft (Call @"Update_operators")
      [ AddOperator OperatorParam
          { opOwner = alice
          , opOperator = toAddress bondingCurve
          , opTokenId = tokenId0
          }
      ]

  withSender alice $
    call bondingCurve (Call @"Sell") tokenId0
      -- & expectError (unsafeMkMText "NO_TOKENS")

  -- ensure tokenId0 burned
  postBurnStorage <- getStorage' nft
  postBurnStorage @== (exampleNftStorageWithAdmin alice) {
    assets = exampleNftTokenStorage {
        next_token_id = TokenId 1
      , operators = [(FA2.OperatorKey
          { owner = bob
          , operator = alice
          , tokenId = tokenId0
          }, ())]
      } }

  -- TODO: ensure expectedPrice sent to alice
  -- let expectedPrice :: Integer = 42
  -- call bondingCurve (Call @"Cost") (0 :: Natural)
  --   & expectError (WrappedValue expectedPrice)





-- sell with token_index = 0 always fails with NO_TOKENS
sellOffchainTokenIndex0 :: TestTree
sellOffchainTokenIndex0 = nettestScenarioOnEmulatorCaps "Sell_offchain: token_index = 0" $ do
  setup <- doFA2Setup
  let admin ::< alice ::< bob ::< SNil = sAddresses setup
  let tokenId0 ::< SNil = sTokens setup
  nft <- originateNft (exampleNftStorageWithAdmin admin)

  let bondingCurveStorage :: Storage =
        (exampleStorageWithAdmin admin)
          {
            market_contract = toAddress nft
          , cost_mutez = constantPiecewisePolynomial 0
          , token_index = 0
          }
  bondingCurve <- originateBondingCurve bondingCurveStorage

  -- mint to alice
  withSender admin $
    call nft (Call @"Mint") [MintTokenParam
      { token_metadata = tokenMetadata0' tokenId0
      , owner = alice
      }]

  withSender admin $
    call bondingCurve (Call @"Sell_offchain") (tokenId0, bob)
      & expectError (unsafeMkMText "NO_TOKENS")



-- TODO: sell-offchain
-- , nettestScenarioCaps "Sell_offchain" $ do
sellOffchainTest :: TestTree
sellOffchainTest = nettestScenarioOnEmulatorCaps "Sell_offchain" $ do
  setup <- doFA2Setup
  let admin ::< alice ::< bob ::< SNil = sAddresses setup
  let tokenId0 ::< SNil = sTokens setup
  nft <- originateNft (exampleNftStorageWithAdmin admin)

  let bondingCurveStorage :: Storage =
        (exampleStorageWithAdmin admin)
          {
            market_contract = toAddress nft
          , cost_mutez = constantPiecewisePolynomial 0
          , token_index = 1 -- token_index > 0 to sell tokens, otherwise no tokens to sell
          }
  bondingCurve <- originateBondingCurve bondingCurveStorage

  -- admin only
  withSender alice $
    call bondingCurve (Call @"Sell_offchain") (tokenId0, alice)
      & expectError (unsafeMkMText "NOT_AN_ADMIN")

  withSender admin $
    call bondingCurve (Call @"Sell_offchain") (tokenId0, alice)
      & expectError (unsafeMkMText "WRONG_ID")

  -- mint to alice
  withSender admin $
    call nft (Call @"Mint") [MintTokenParam
      { token_metadata = tokenMetadata0' tokenId0
      , owner = alice
      }]

  -- bob can't sell alice's token
  withSender bob $
    call bondingCurve (Call @"Sell") tokenId0
      & expectError (unsafeMkMText "WRONG_SYMBOL")

  -- admin can't sell alice's tokenId0 "from bob"
  withSender admin $
    call bondingCurve (Call @"Sell_offchain") (tokenId0, bob)
      & expectError (unsafeMkMText "WRONG_SYMBOL")

  withSender admin $
    call bondingCurve (Call @"Sell_offchain") (tokenId0, alice)
      & expectError (unsafeMkMText "WRONG_TEZ_PRICE")

  -- ensure tokenId0 burned
  postBurnStorage <- getStorage' nft
  postBurnStorage @== (exampleNftStorageWithAdmin alice) {
    assets = exampleNftTokenStorage {
        next_token_id = TokenId 1
      , operators = [(FA2.OperatorKey
          { owner = bob
          , operator = alice
          , tokenId = tokenId0
          }, ())]
      } }

  -- TODO: ensure expectedPrice sent to alice



test_Integrational :: TestTree
test_Integrational = testGroup "Integrational"
  [

  -- TODO: re-enable
  --   setDelegateTest
  -- , withdrawTest
  -- , buyNoMint

    buyTest
  , buyOffchainTest

  -- , sellTokenIndex0
  , sellTest

  -- , sellOffchainTokenIndex0
  , sellOffchainTest
  ]

-- input, expectedOutput, storageF
--
-- storageF is applied to the generated admin address
callCostTest :: Natural -> Integer -> (Address -> Storage) -> TestTree
callCostTest input expectedOutput storageF =
  nettestScenarioCaps ("Call Cost with " ++ show input) $ do
    setup <- doFA2Setup @("addresses" :# 1) @("tokens" :# 0)
    let admin ::< SNil = sAddresses setup
    let bondingCurveStorage = storageF admin
    bondingCurve <- originateDebugBondingCurve bondingCurveStorage

    call bondingCurve (Call @"Cost") input
      & expectError (WrappedValue expectedOutput)


-- TODO: re-enable
-- -- test cost function using the debug version of the contract
-- test_Debug :: TestTree
-- test_Debug = testGroup "Debug"
--   [ -- default storage cost_mutez(4) == 34
--     callCostTest 4 39 exampleStorageWithAdmin

--   -- (constantPiecewisePolynomial 0) cost_mutez(12) == 0
--   , callCostTest 12 0 (\admin -> (exampleStorageWithAdmin admin)
--       { cost_mutez = constantPiecewisePolynomial 0 })

--   ]

