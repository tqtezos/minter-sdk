{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE InstanceSigs #-}

-- | Tests for bonding curve contract
module Test.BondingCurve where

import Prelude hiding (swap)
import System.IO (writeFile)

import qualified Data.Text.Lazy as L
import Test.Tasty (TestTree, testGroup)

import Lorentz.Value
import Michelson.Printer
import Michelson.Text (unsafeMkMText)
import Michelson.Typed.Scope () -- (ConstantScope)
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

-- Test SimpleAdmin admin ownership transfer
test_AdminChecks :: TestTree
test_AdminChecks =
  adminOwnershipTransferChecks @Entrypoints  @Storage
    (\admin ->
      originateBondingCurve
        (exampleStorageWithAdmin admin)
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

withdrawTest :: TestTree
withdrawTest = nettestScenarioCaps "Withdraw" $ do
  setup <- doFA2Setup
  let admin ::< alice ::< SNil = sAddresses setup
  let !SNil = sTokens setup

  -- ensure admin has no tez
  withSender admin $
    getBalance admin >>= transferMoney alice
  getBalance admin @@== 0

  let withdrawAmount = 1234
  let bondingCurveStorage :: Storage =
        (exampleStorageWithAdmin admin)
          {
            market_contract = alice
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


buyNoMintTest :: TestTree
buyNoMintTest = nettestScenarioCaps "Buy: NO_MINT" $ do
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


-- sell with token_index = 0 always fails with NO_TOKENS
sellTokenIndex0Test :: TestTree
sellTokenIndex0Test = nettestScenarioOnEmulatorCaps "Sell: token_index = 0" $ do
  setup <- doFA2Setup
  let admin ::< alice ::< SNil = sAddresses setup
  let !SNil = sTokens setup
  nft <- originateNft (exampleNftStorageWithAdmin admin)
  let bondingCurveStorage :: Storage =
        (exampleStorageWithAdmin admin)
          {
            market_contract = toAddress nft
          , token_index = 0
          }
  bondingCurve <- originateBondingCurve bondingCurveStorage

  withSender admin $
    call bondingCurve (Call @"Sell") (TokenId 0)
      & expectError (unsafeMkMText "NO_TOKENS")

  withSender alice $
    call bondingCurve (Call @"Sell") (TokenId 0)
      & expectError (unsafeMkMText "NO_TOKENS")


-- sell with token_index = 0 always fails with NO_TOKENS
sellOffchainTokenIndex0Test :: TestTree
sellOffchainTokenIndex0Test = nettestScenarioOnEmulatorCaps "Sell_offchain: token_index = 0" $ do
  setup <- doFA2Setup
  let admin ::< alice ::< SNil = sAddresses setup
  let !SNil = sTokens setup
  nft <- originateNft (exampleNftStorageWithAdmin admin)
  let bondingCurveStorage :: Storage =
        (exampleStorageWithAdmin admin)
          {
            market_contract = toAddress nft
          , cost_mutez = constantPiecewisePolynomial 0
          , token_index = 0
          }
  bondingCurve <- originateBondingCurve bondingCurveStorage

  withSender admin $
    call bondingCurve (Call @"Sell_offchain") (TokenId 0, admin)
      & expectError (unsafeMkMText "NO_TOKENS")

  withSender admin $
    call bondingCurve (Call @"Sell_offchain") (TokenId 0, alice)
      & expectError (unsafeMkMText "NO_TOKENS")




--------------------------------------------------------------------------------
-- TESTS ABOVE PASSING
--------------------------------------------------------------------------------


--- too little/much tez
--- Spec:
--  + Mints token using `token_metadata` from storage to buyer
--  + Increments `token_index`
--  + Adds the `basis_points` fee to the `unclaimed` tez in storage
buyTest :: TestTree
-- buyTest = nettestScenarioCaps "Buy" $ do
buyTest = nettestScenarioOnEmulatorCaps "Buy" $ do
  setup <- doFA2Setup
  let admin ::< alice ::< SNil = sAddresses setup
  let !SNil = sTokens setup
  nft <- originateNft ((exampleNftStorageWithAdmin admin)
    { assets = exampleNftTokenStorage { ledger = [(TokenId 0, admin)]
                                      , next_token_id = TokenId 1
                                      } })

  let bondingCurveStorage :: Storage =
        (exampleStorageWithAdmin admin)
          {
            market_contract = toAddress nft
          , cost_mutez = constantPiecewisePolynomial 0
          , auction_price = 10
          }
  bondingCurve <- originateDebugBondingCurve bondingCurveStorage

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
      & expectError (unsafeMkMText "WRONG_TEZ_PRICE")

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



buyOffchainTest :: TestTree
buyOffchainTest = nettestScenarioOnEmulatorCaps "Buy_offchain" $ do
  setup <- doFA2Setup
  let admin ::< alice ::< bob ::< SNil = sAddresses setup
  let !SNil = sTokens setup
  nft <- originateNft ((exampleNftStorageWithAdmin admin)
    { assets = exampleNftTokenStorage { ledger = [(TokenId 0, admin)]
                                      , next_token_id = TokenId 1
                                      } })

  let bondingCurveStorage :: Storage =
        (exampleStorageWithAdmin admin)
          {
            market_contract = toAddress nft
          , cost_mutez = constantPiecewisePolynomial 0
          , token_metadata = tokenMetadata0
          }
  bondingCurve <- originateBondingCurve bondingCurveStorage

  -- admin needs to set operator on (TokenId 0) to allow bondingCurve to mint
  withSender admin $
    call nft (Call @"Update_operators")
      [ AddOperator OperatorParam
          { opOwner = admin
          , opOperator = toAddress bondingCurve
          , opTokenId = TokenId 0
          }
      ]

  -- admin only
  withSender alice $
    call bondingCurve (Call @"Buy_offchain") alice
      & expectError (unsafeMkMText "NOT_AN_ADMIN")

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


buyBatchOffchainTest :: TestTree
buyBatchOffchainTest = nettestScenarioOnEmulatorCaps "Buy_offchain" $ do
  setup <- doFA2Setup
  let admin ::< alice ::< bob ::< SNil = sAddresses setup
  let !SNil = sTokens setup
  nft <- originateNft ((exampleNftStorageWithAdmin admin)
    { assets = exampleNftTokenStorage { ledger = [(TokenId 0, admin)]
                                      , next_token_id = TokenId 1
                                      } })

  let bondingCurveStorage :: Storage =
        (exampleStorageWithAdmin admin)
          {
            market_contract = toAddress nft
          , cost_mutez = constantPiecewisePolynomial 0
          }
  bondingCurve <- originateBondingCurve bondingCurveStorage

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
  postBuyStorage @== bondingCurveStorage





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
  let admin ::< minter ::< alice ::< bob ::< SNil = sAddresses setup
  let !SNil = sTokens setup
  nft <- originateNft ((exampleNftStorageWithAdmin admin)
    { assets = exampleNftTokenStorage { ledger = [(TokenId 0, admin)]
                                      , next_token_id = TokenId 1
                                      } })

  let bondingCurveStorage :: Storage =
        (exampleStorageWithAdmin admin)
          {
            market_contract = toAddress nft
          , cost_mutez = constantPiecewisePolynomial 0
          , token_index = 1 -- token_index must be > 0 to sell
          , token_metadata = tokenMetadata0
          }
  bondingCurve <- originateBondingCurve bondingCurveStorage

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



sellOffchainTest :: TestTree
sellOffchainTest = nettestScenarioOnEmulatorCaps "Sell_offchain" $ do
  setup <- doFA2Setup
  let admin ::< minter ::< alice ::< bob ::< SNil = sAddresses setup
  let !SNil = sTokens setup
  nft <- originateNft ((exampleNftStorageWithAdmin admin)
    { assets = exampleNftTokenStorage { ledger = [(TokenId 0, admin)]
                                      , next_token_id = TokenId 1
                                      } })

  let bondingCurveStorage :: Storage =
        (exampleStorageWithAdmin admin)
          {
            market_contract = toAddress nft
          , cost_mutez = constantPiecewisePolynomial 10
          , token_index = 1 -- token_index > 0 to sell tokens, otherwise no tokens to sell
          , token_metadata = tokenMetadata0
          }
  bondingCurve <- originateBondingCurveWithBalance 10 bondingCurveStorage

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


buySellTest :: TestTree
buySellTest = nettestScenarioOnEmulatorCaps "Buy Sell" $ do

  let logFile = "buy_sell_test_data.txt"
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
  let bondingCurveStorage :: Storage =
        (exampleStorageWithAdmin admin)
          {
            market_contract = toAddress nft
          , cost_mutez = polynomialToPiecewisePolynomial [10, 20, 30]
          , auction_price = auctionPrice
          , basis_points = basisPoints
          }
  log "bonding curve storage"
  log . L.toStrict . printTypedValue dontForceSingleLine $ toVal bondingCurveStorage
  log ""

  bondingCurve <- originateDebugBondingCurve bondingCurveStorage
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

    -- basis_points fee required
    withSender buyer $
      transfer (
        TransferData
          { tdTo = bondingCurve
          , tdAmount = fromIntegral $ fromIntegral auctionPrice + amount
          , tdEntrypoint = ep "buy"
          , tdParameter = ()
          })
        & expectError (unsafeMkMText "WRONG_TEZ_PRICE")

    let buyAmount = fromIntegral . addBasisPointFee 100 $ fromIntegral auctionPrice + amount
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

    -- ensure cost was expected
    sellerBalanceAfter <- getBalance seller
    (tokenId, (sellerBalanceAfter - sellerBalanceBefore)) @== (tokenId, auctionPrice + fromIntegral expectedCost)

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


buySellOffchainTest :: TestTree
buySellOffchainTest = nettestScenarioOnEmulatorCaps "Buy Sell Offchain" $ do
  setup <- doFA2Setup
  let admin ::< alice ::< bob ::< charlie ::< SNil = sAddresses setup
  let !SNil = sTokens setup
  nft <- originateNft ((exampleNftStorageWithAdmin admin)
    { assets = exampleNftTokenStorage { ledger = [(TokenId 0, admin)]
                                      , next_token_id = TokenId 1
                                      } })

  let auctionPrice = 100
  let basisPoints = 100
  let bondingCurveStorage :: Storage =
        (exampleStorageWithAdmin admin)
          {
            market_contract = toAddress nft
          , cost_mutez = polynomialToPiecewisePolynomial [10, 20, 30]
          , auction_price = auctionPrice
          , basis_points = basisPoints
          }

  bondingCurve <- originateDebugBondingCurve bondingCurveStorage

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

    -- basis_points fee required
    withSender admin $
      transfer (
        TransferData
          { tdTo = bondingCurve
          , tdAmount = fromIntegral $ fromIntegral auctionPrice + amount
          , tdEntrypoint = ep "buy_offchain"
          , tdParameter = buyer
          })
        & expectError (unsafeMkMText "WRONG_TEZ_PRICE")

    withSender admin $
      transfer $
        TransferData
          { tdTo = bondingCurve
          , tdAmount = fromIntegral . addBasisPointFee 100 $ fromIntegral auctionPrice + amount
          , tdEntrypoint = ep "buy_offchain"
          , tdParameter = buyer
          }

  let sellers :: [(Natural, (Integer, Address))] = zip [1..] buyers

  forM_ (reverse sellers) $ \(tokenId, (expectedCost, seller)) -> do
    sellerBalanceBefore <- getBalance seller

    withSender admin $
      call bondingCurve (Call @"Sell_offchain") (TokenId tokenId, seller)

    -- ensure cost was expected
    sellerBalanceAfter <- getBalance seller
    (tokenId, (sellerBalanceAfter - sellerBalanceBefore)) @== (tokenId, auctionPrice + fromIntegral expectedCost)

  -- ensure zero tokens remaining and unclaimed is expected
  postSellStorage <- getStorage' bondingCurve
  postSellStorage @== bondingCurveStorage { unclaimed = 4 }

  withSender admin $
    call bondingCurve (Call @"Withdraw") ()

  getBalance bondingCurve @@== 0

  -- ensure storage reset after all tokens sold and Withdraw is called
  postWithdrawStorage <- getStorage' bondingCurve
  postWithdrawStorage @== bondingCurveStorage



test_Integrational :: TestTree
test_Integrational = testGroup "Integrational"
  [ withdrawTest
  , buyNoMintTest

  , buyTest
  , buyOffchainTest

  , sellTokenIndex0Test
  , sellTest

  , sellOffchainTokenIndex0Test
  , sellOffchainTest

  , buySellTest
  , buySellOffchainTest
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


-- test cost function using the debug version of the contract
test_Debug :: TestTree
test_Debug = testGroup "Debug"
  [ -- default storage cost_mutez(4) == 34
    callCostTest 4 39 exampleStorageWithAdmin

  -- (constantPiecewisePolynomial 0) cost_mutez(12) == 0
  , callCostTest 12 0 (\admin -> (exampleStorageWithAdmin admin)
      { cost_mutez = constantPiecewisePolynomial 0 })
  ]

