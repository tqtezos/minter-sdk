module Test.Marketplace.FA2FixedFee where

import qualified Data.Map as Map
import Hedgehog (Gen, Property, forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Morley.Nettest

import Lorentz hiding (balance, contract)

import qualified Indigo.Contracts.FA2Sample as FA2
import Lorentz.Contracts.Marketplace.Contracts
import Lorentz.Contracts.Marketplace.FA2FixedFee
import Lorentz.Contracts.MinterSdk
import Lorentz.Contracts.PausableAdminOption
import Lorentz.Contracts.Spec.FA2Interface (TokenId(TokenId))
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Test.Util

hprop_Every_sale_sends_a_fee_to_the_fee_collector :: Property
hprop_Every_sale_sends_a_fee_to_the_fee_collector =
  property $ do
    testData@TestData{testSalePrice, testTokenAmount, testFeePercent} <- forAll genTestData
    let expectedFee = testSalePrice * testFeePercent `div` 100
    clevelandProp $ do
      setup@Setup{seller, buyer, feeCollector, moneyFA2} <- testSetup testData
      contract <- originateMarketplaceContract setup

      withSender seller $
        sell testData setup contract

      -- buy each and every token up for sale
      withSender buyer $
        replicateM_ (fromIntegral testTokenAmount) $ do
          feeCollectorsInitialBalance <- balanceOf moneyFA2 moneyTokenId feeCollector
          buy contract
          feeCollectorsFinalBalance <- balanceOf moneyFA2 moneyTokenId feeCollector

          feeCollectorsFinalBalance @== feeCollectorsInitialBalance + expectedFee

hprop_Tokens_are_transferred_to_seller :: Property
hprop_Tokens_are_transferred_to_seller  =
  property $ do
    testData@TestData{testSalePrice, testTokenAmount, testFeePercent} <- forAll genTestData
    let expectedFee = testSalePrice * testFeePercent `div` 100
    let expectedProfitPerToken = testSalePrice - expectedFee
    let expectedProfit = expectedProfitPerToken * testTokenAmount

    clevelandProp $ do
      setup@Setup{seller, buyer, moneyFA2} <- testSetup testData
      contract <- originateMarketplaceContract setup

      withSender seller $
        sell testData setup contract

      -- buy each and every token up for sale
      withSender buyer $
        replicateM_ (fromIntegral testTokenAmount) $ do
          sellerBalanceBeforeSale <- balanceOf moneyFA2 moneyTokenId seller
          buy contract
          sellerBalanceAfterSale <- balanceOf moneyFA2 moneyTokenId seller
          sellerBalanceAfterSale @== sellerBalanceBeforeSale + expectedProfitPerToken

      sellerBalance <- balanceOf moneyFA2 moneyTokenId seller
      sellerBalance @== expectedProfit

hprop_Assets_are_transferred_to_buyer :: Property
hprop_Assets_are_transferred_to_buyer  =
  property $ do
    testData@TestData{testTokenAmount} <- forAll genTestData
    clevelandProp $ do
      setup@Setup{seller, buyer, assetFA2} <- testSetup testData
      contract <- originateMarketplaceContract setup

      withSender seller $
        sell testData setup contract
      withSender buyer $
        forM_ [1 .. testTokenAmount] \n -> do
          buy contract
          buyerAssetBalance <- balanceOf assetFA2 assetTokenId buyer
          buyerAssetBalance @== n

      sellerAssetBalance <- balanceOf assetFA2 assetTokenId seller
      sellerAssetBalance @== 0

hprop_Contracts_balance_is_zero_after_a_sale_is_concluded :: Property
hprop_Contracts_balance_is_zero_after_a_sale_is_concluded =
  property $ do
    testData <- forAll genTestData

    clevelandProp $ do
      setup@Setup{seller, buyer, assetFA2, moneyFA2} <- testSetup testData
      contract <- originateMarketplaceContract setup

      withSender seller $
        sell testData setup contract
      withSender buyer $
        buyAll testData contract

      contractTokenBalance <- balanceOf moneyFA2 moneyTokenId contract
      contractAssetBalance <- balanceOf assetFA2 assetTokenId contract
      contractTezBalance <- getBalance contract
      contractTokenBalance @== 0
      contractAssetBalance @== 0
      contractTezBalance @== 0

hprop_Global_balance_is_conserved :: Property
hprop_Global_balance_is_conserved =
  property $ do
    testData <- forAll genTestData

    clevelandProp $ do
      setup@Setup{seller, buyer, moneyFA2, feeCollector} <- testSetup testData
      contract <- originateMarketplaceContract setup

      initialGlobalBalance <- sum <$> forM [feeCollector, buyer, seller] \account ->
        balanceOf moneyFA2 moneyTokenId account

      withSender seller $
        sell testData setup contract
      withSender buyer $
        buyAll testData contract

      finalGlobalBalance <- sum <$> forM [feeCollector, buyer, seller] \account ->
        balanceOf moneyFA2 moneyTokenId account

      finalGlobalBalance @== initialGlobalBalance

hprop_Assets_are_held_in_escrow :: Property
hprop_Assets_are_held_in_escrow =
  property $ do
    testData@TestData{testTokenAmount} <- forAll genTestData

    clevelandProp $ do
      setup@Setup{seller, assetFA2} <- testSetup testData
      contract <- originateMarketplaceContract setup

      withSender seller $
        sell testData setup contract

      contractAssetBalance <- balanceOf assetFA2 assetTokenId contract
      contractAssetBalance @== testTokenAmount

      sellerAssetBalance <- balanceOf assetFA2 assetTokenId seller
      sellerAssetBalance @== 0

hprop_Cant_sell_if_fee_is_too_high :: Property
hprop_Cant_sell_if_fee_is_too_high  =
  property $ do
    invalidFeePercent <- forAll $ Gen.integral (Range.linear 101 10000)
    testData <- forAll genTestData <&> \testData -> testData { testFeePercent = invalidFeePercent }

    clevelandProp $ do
      setup@Setup{seller} <- testSetup testData
      contract <- originateMarketplaceContract setup

      withSender seller $ do
        sell testData setup contract
          & expectTransferFailure [failedWith $ constant [mt|FEE_TOO_HIGH|]]

hprop_Cant_buy_if_fee_is_too_high :: Property
hprop_Cant_buy_if_fee_is_too_high  =
  property $ do
    invalidFeePercent <- forAll $ Gen.integral (Range.linear 101 10000)

    -- Must sell at least 1 token
    testTokenAmount <- forAll $ Gen.integral (Range.constant 1 10)

    testData <- forAll genTestData <&> \testData -> testData
      { testTokenAmount = testTokenAmount
      , testFeePercent = invalidFeePercent
      }

    clevelandProp $ do
      -- Originate the contract with a "sale" entry in the initial storage
      setup@Setup{seller, buyer, assetFA2} <- addSaleToInitialStorage testData <$> testSetup testData
      contract <- originateMarketplaceContract setup

      -- Transfer the asset to the contract
      withSender seller $ do
        call assetFA2 (Call @"Transfer")
          [ FA2.TransferItem
              { tiFrom = seller
              , tiTxs = one FA2.TransferDestination
                  { tdTo = toAddress contract
                  , tdTokenId = assetTokenId
                  , tdAmount = testTokenAmount
                  }
              }
          ]

      -- Attempt to buy the asset held in the contract
      withSender buyer $ do
        buy contract
          & expectTransferFailure [failedWith $ constant [mt|FEE_TOO_HIGH|]]

  where
    addSaleToInitialStorage :: TestData -> Setup -> Setup
    addSaleToInitialStorage TestData{testSalePrice, testTokenAmount} setup@Setup{storage, seller, assetFA2, moneyFA2} =
      setup
        { storage = storage
            { sales = BigMap $ Map.fromList
                [ ( SaleId 0
                  , SaleParam
                    { seller = seller
                    , saleData = SaleData
                      { salePricePerToken = testSalePrice
                      , saleToken = SaleToken
                        { fa2Address = toAddress assetFA2
                        , tokenId = assetTokenId
                        }
                      , moneyToken = MoneyToken
                        { fa2Address = toAddress moneyFA2
                        , tokenId = moneyTokenId
                        }
                      , tokenAmount = testTokenAmount
                      }
                    }
                  )
                ]
            }
        }

hprop_Cancelling_a_sale_returns_the_remaining_assets_back_to_seller :: Property
hprop_Cancelling_a_sale_returns_the_remaining_assets_back_to_seller =
  property $ do
    testData@TestData{testTokenAmount} <- forAll genTestData

    -- Buy some of the tokens (but not all of them!)
    amountToBuy <-
      if testTokenAmount == 0
        then pure 0
        else forAll $ Gen.integral (Range.linear 0 (testTokenAmount - 1))

    clevelandProp $ do
      setup@Setup{seller, buyer, assetFA2} <- testSetup testData
      contract <- originateMarketplaceContract setup

      withSender seller $ do
        sell testData setup contract

      withSender buyer $ do
        replicateM (fromIntegral amountToBuy) $ do
          buy contract

      withSender seller $ do
        cancel contract

      contractAssetBalance <- balanceOf assetFA2 assetTokenId contract
      contractAssetBalance @== 0

      sellerAssetBalance <- balanceOf assetFA2 assetTokenId seller
      sellerAssetBalance @== testTokenAmount - amountToBuy

hprop_Cancelling_a_sale_deletes_it_from_storage :: Property
hprop_Cancelling_a_sale_deletes_it_from_storage =
  property $ do
    testData <- forAll genTestData
    clevelandProp $ do
      setup@Setup{seller} <- testSetup testData
      contract <- originateMarketplaceContract setup

      withSender seller $ do
        sell testData setup contract
        cancel contract

      storage <- getStorage' contract
      sales storage @== mempty

hprop_Cant_buy_more_assets_than_are_available :: Property
hprop_Cant_buy_more_assets_than_are_available =
  property $ do
    testData@TestData{testTokenAmount} <- forAll genTestData

    -- Make sure the buy has enough balance to buy n+1 tokens
    -- (where n is the amount of tokens available).
    let testData' =
          testData
            { testBuyersBalance = testBuyersBalance testData + testSalePrice testData
            }

    clevelandProp $ do
      setup@Setup{seller, buyer} <- testSetup testData'
      contract <- originateMarketplaceContract setup

      withSender seller $ do
        sell testData' setup contract
      withSender buyer $ do
        buyAll testData' contract

        buy contract
          & expectTransferFailure
              if testTokenAmount == 0
              then [ failedWith $ constant
                      ([mt|FA2_INSUFFICIENT_BALANCE|], 1 :: Natural, 0 :: Natural) ]
              else [ failedWith $ constant [mt|NO_SALE|] ]

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

assetTokenId, moneyTokenId :: TokenId
assetTokenId = TokenId 1
moneyTokenId = TokenId 2

data TestData = TestData
  { testFeePercent :: Natural
  , testTokenAmount :: Natural -- ^ Amount of tokens to sell
  , testSalePrice :: Natural -- ^ The price of the NFT / each FT being sold.
  , testBuyersBalance :: Natural
  }
  deriving stock (Show)

genTestData :: Gen TestData
genTestData = do
  feePercent <- Gen.integral (Range.linear 0 100)
  tokenAmount <- Gen.integral (Range.constant 0 10)
  salePrice <- Gen.integral (Range.linear 0 10000)

  -- Make sure the buyer has enough balance to make the purchase.
  let minBuyersBalance = salePrice * tokenAmount
  buyersBalance <- Gen.integral (Range.linear minBuyersBalance (2 * minBuyersBalance))

  pure $ TestData
    { testFeePercent = feePercent
    , testTokenAmount = tokenAmount
    , testSalePrice = salePrice
    , testBuyersBalance = buyersBalance
    }

data Setup = Setup
  { seller :: Address
  , buyer :: Address
  , feeCollector :: Address
  , assetFA2 :: ContractHandler FA2.FA2SampleParameter FA2.Storage
  , moneyFA2 :: ContractHandler FA2.FA2SampleParameter FA2.Storage
  , storage :: MarketplaceStorage ()
  }

testSetup :: MonadNettest caps base m => TestData -> m Setup
testSetup testData = do
  seller <- newAddress "seller"
  buyer <- newAddress "buyer"
  feeCollector <- newAddress "fee-collector"

  let storage =
        MarketplaceStorage
          { admin = Just AdminStorage
              { admin = seller
              , pendingAdmin = Nothing
              , paused = False
              }
          , sales = mempty
          , nextSaleId = SaleId 0
          , allowlist = ()
          , fee = FeeData
            { feeAddress = feeCollector
            , feePercent = testFeePercent testData
            }
          }

  -- Create two FA2 contracts, and give the asset to the seller and some tokens to the buyer.
  assetFA2 <- originateSimple "asset_fa2"
    FA2.Storage
    { sLedger = BigMap $ Map.fromList [((seller, assetTokenId), testTokenAmount testData)]
    , sOperators = mempty
    , sTokenMetadata = mempty
    }
    (FA2.fa2Contract def { FA2.cAllowedTokenIds = [assetTokenId] })
  moneyFA2 <- originateSimple "money_fa2"
    FA2.Storage
    { sLedger = BigMap $ Map.fromList [((buyer, moneyTokenId), testBuyersBalance testData)]
    , sOperators = mempty
    , sTokenMetadata = mempty
    }
    (FA2.fa2Contract def { FA2.cAllowedTokenIds = [moneyTokenId] })
  pure Setup {..}

originateMarketplaceContract :: MonadNettest caps base m => Setup -> m (ContractHandler (MarketplaceEntrypoints Never) (MarketplaceStorage ()))
originateMarketplaceContract Setup{storage, seller, buyer, assetFA2, moneyFA2} = do
  contract <- originateSimple "marketplace-fa2-fixed-fee" storage marketplaceFixedFeeContract

  -- Make the contract an operator for the seller and the buyer.
  withSender seller $ do
    call assetFA2 (Call @"Update_operators")
      [ FA2.AddOperator FA2.OperatorParam
          { opOwner = seller
          , opOperator = toAddress contract
          , opTokenId = assetTokenId
          }
      ]

  withSender buyer $ do
    call moneyFA2 (Call @"Update_operators")
      [ FA2.AddOperator FA2.OperatorParam
          { opOwner = buyer
          , opOperator = toAddress contract
          , opTokenId = moneyTokenId
          }
      ]

  pure contract

----------------------------------------------------------------------------
-- Call entrypoints
----------------------------------------------------------------------------

sell :: (HasCallStack, MonadNettest caps base m) => TestData -> Setup -> ContractHandler (MarketplaceEntrypoints Never) storage -> m ()
sell TestData{testSalePrice, testTokenAmount} Setup{assetFA2, moneyFA2} contract =
  call contract (Call @"Sell") SaleData
    { salePricePerToken = testSalePrice
    , saleToken = SaleToken
      { fa2Address = toAddress assetFA2
      , tokenId = assetTokenId
      }
    , moneyToken = MoneyToken
      { fa2Address = toAddress moneyFA2
      , tokenId = moneyTokenId
      }
    , tokenAmount = testTokenAmount
    }

buy :: (HasCallStack, MonadNettest caps base m) => ContractHandler (MarketplaceEntrypoints Never) storage -> m ()
buy contract =
  call contract (Call @"Buy") (SaleId 0)

buyAll :: (HasCallStack, MonadNettest caps base m) => TestData -> ContractHandler (MarketplaceEntrypoints Never) storage -> m ()
buyAll TestData{testTokenAmount} contract =
  replicateM_ (fromIntegral testTokenAmount) $
    buy contract

cancel :: (HasCallStack, MonadNettest caps base m) => ContractHandler (MarketplaceEntrypoints Never) storage -> m ()
cancel contract =
  call contract (Call @"Cancel") (SaleId 0)
