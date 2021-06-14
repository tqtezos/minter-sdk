module Test.Marketplace.TezFixedFee where

import qualified Data.Map as Map
import Hedgehog (Gen, Property, forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Morley.Nettest

import Lorentz hiding (balance, contract)
import Michelson.Typed (convertContract, untypeValue)

import qualified Indigo.Contracts.FA2Sample as FA2
import Lorentz.Contracts.Marketplace.TezFixedFee
import Lorentz.Contracts.MinterSdk
import Lorentz.Contracts.PausableAdminOption
import Lorentz.Contracts.Spec.FA2Interface (TokenId(TokenId))
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Test.Util

hprop_Every_sale_sends_a_fee_to_the_fee_collector :: Property
hprop_Every_sale_sends_a_fee_to_the_fee_collector =
  property $ do
    testData@TestData{testSalePrice, testTokenAmount, testFeePercent} <- forAll genTestData
    let expectedFee = (testSalePrice * fromIntegral testFeePercent) `div` 100

    clevelandProp $ do
      setup@Setup{seller, buyer, feeCollector} <- testSetup testData
      contract <- originateMarketplaceContract setup

      withSender seller $
        sell testData setup contract

      -- buy each and every token up for sale
      withSender buyer $
        replicateM_ (fromIntegral testTokenAmount) $ do
          feeCollectorInitialBalance <- getBalance feeCollector
          buy testData contract
          feeCollectorFinalBalance <- getBalance feeCollector

          feeCollectorFinalBalance @== feeCollectorInitialBalance + expectedFee

hprop_Tez_is_transferred_to_seller :: Property
hprop_Tez_is_transferred_to_seller  =
  property $ do
    testData@TestData{testSalePrice, testTokenAmount, testFeePercent} <- forAll genTestData
    let expectedFee = (testSalePrice * fromIntegral testFeePercent) `div` 100
    let expectedProfitPerToken = testSalePrice - expectedFee
    let expectedProfit = expectedProfitPerToken * fromIntegral testTokenAmount

    clevelandProp $ do
      setup@Setup{seller, buyer} <- testSetup testData
      contract <- originateMarketplaceContract setup
      sellerInitialBalance <- getBalance seller

      withSender seller $
        sell testData setup contract

      -- buy each and every token up for sale
      withSender buyer $
        replicateM_ (fromIntegral testTokenAmount) $ do
          sellerBalanceBeforeSale <- getBalance seller
          buy testData contract
          sellerBalanceAfterSale <- getBalance seller
          sellerBalanceAfterSale @== sellerBalanceBeforeSale + expectedProfitPerToken

      sellerFinalBalance <- getBalance seller
      sellerFinalBalance @== sellerInitialBalance + expectedProfit

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
          buy testData contract
          buyerAssetBalance <- balanceOf assetFA2 assetTokenId buyer
          buyerAssetBalance @== n

      sellerAssetBalance <- balanceOf assetFA2 assetTokenId seller
      sellerAssetBalance @== 0

hprop_Contracts_balance_is_zero_after_a_sale_is_concluded :: Property
hprop_Contracts_balance_is_zero_after_a_sale_is_concluded =
  property $ do
    testData <- forAll genTestData

    clevelandProp $ do
      setup@Setup{seller, buyer, assetFA2} <- testSetup testData
      contract <- originateMarketplaceContract setup

      withSender seller $
        sell testData setup contract
      withSender buyer $
        buyAll testData contract

      contractAssetBalance <- balanceOf assetFA2 assetTokenId contract
      contractTezBalance <- getBalance contract
      contractAssetBalance @== 0
      contractTezBalance @== 0

hprop_Global_balance_is_conserved :: Property
hprop_Global_balance_is_conserved =
  property $ do
    testData <- forAll genTestData

    clevelandProp $ do
      setup@Setup{seller, buyer, feeCollector} <- testSetup testData
      contract <- originateMarketplaceContract setup

      initialGlobalBalance <- sum <$> forM [feeCollector, buyer, seller] \account ->
        getBalance account

      withSender seller $
        sell testData setup contract
      withSender buyer $
        buyAll testData contract

      finalGlobalBalance <- sum <$> forM [feeCollector, buyer, seller] \account ->
        getBalance account

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
        sell testData setup contract `expectFailure` failedWith contract [mt|FEE_TOO_HIGH|]

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
        buy testData contract `expectFailure` failedWith contract [mt|FEE_TOO_HIGH|]
  where
    addSaleToInitialStorage :: TestData -> Setup -> Setup
    addSaleToInitialStorage TestData{testSalePrice, testTokenAmount} setup@Setup{storage, seller, assetFA2} =
      setup
        { storage = storage
            { sales = BigMap $ Map.fromList
                [ (SaleId 0
                  , SaleParamTez
                    { seller = seller
                    , saleDataTez = SaleDataTez
                      { saleToken = SaleToken
                        { fa2Address = toAddress assetFA2
                        , tokenId = assetTokenId
                        }
                      , salePricePerToken = testSalePrice
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
          buy testData contract

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

      storage <- fromVal @(MarketplaceTezStorage ()) <$> getStorage' contract
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
      setup@Setup{seller, buyer, assetFA2} <- testSetup testData'
      contract <- originateMarketplaceContract setup

      withSender seller $ do
        sell testData' setup contract
      withSender buyer $ do
        buyAll testData' contract

        buy testData' contract `expectFailure`
          if testTokenAmount == 0
            then failedWith assetFA2 ([mt|FA2_INSUFFICIENT_BALANCE|], 1 :: Natural, 0 :: Natural)
            else failedWith contract [mt|NO_SALE|]

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

assetTokenId :: TokenId
assetTokenId = TokenId 1

data TestData = TestData
  { testFeePercent :: Natural
  , testTokenAmount :: Natural -- ^ Amount of tokens to sell
  , testSalePrice :: Mutez -- ^ The price of the NFT / each FT being sold.
  , testBuyersBalance :: Mutez
  }
  deriving stock (Show)

genTestData :: Gen TestData
genTestData = do
  feePercent <- Gen.integral (Range.linear 0 100)
  tokenAmount <- Gen.integral (Range.constant 0 10)
  salePrice <- genMutez' (Range.linear 0 10000)

  -- Make sure the buyer has enough balance to make the purchase.
  let minBuyersBalance = salePrice * fromIntegral tokenAmount
  buyersBalance <- genMutez' (Range.linear minBuyersBalance (2 * minBuyersBalance))

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
  , assetFA2 :: TAddress FA2.FA2SampleParameter
  , storage :: MarketplaceTezStorage ()
  }

testSetup :: MonadNettest caps base m => TestData -> m Setup
testSetup testData = do
  seller <- newAddress "seller"
  buyer <- newAddress "buyer"
  feeCollector <- newAddress "fee-collector"

  let storage =
        MarketplaceTezStorage
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

  -- Create an FA2 contracts, and give the asset to the seller.
  assetFA2 <- originateSimple "asset_fa2"
    FA2.Storage
    { sLedger = BigMap $ Map.fromList [((seller, assetTokenId), testTokenAmount testData)]
    , sOperators = mempty
    , sTokenMetadata = mempty
    }
    (FA2.fa2Contract def { FA2.cAllowedTokenIds = [assetTokenId] })

  pure Setup {..}

originateMarketplaceContract :: MonadNettest caps base m => Setup -> m (TAddress $ MarketplaceTezEntrypoints ())
originateMarketplaceContract Setup{storage, seller, assetFA2} = do
  contract <- TAddress @(MarketplaceTezEntrypoints ()) <$> originateUntypedSimple "marketplace-tez-fixed-fee"
    (untypeValue $ toVal storage)
    (convertContract marketplaceTezFixedFeeContract)

  -- Make the contract an operator for the seller.
  withSender seller $ do
    call assetFA2 (Call @"Update_operators")
      [ FA2.AddOperator FA2.OperatorParam
          { opOwner = seller
          , opOperator = toAddress contract
          , opTokenId = assetTokenId
          }
      ]

  pure contract

----------------------------------------------------------------------------
-- Call entrypoints
----------------------------------------------------------------------------

sell :: (HasCallStack, MonadNettest caps base m) => TestData -> Setup -> TAddress (MarketplaceTezEntrypoints ()) -> m ()
sell TestData{testSalePrice, testTokenAmount} Setup{assetFA2} contract =
  call contract (Call @"Sell") SaleDataTez
    { saleToken = SaleToken
        { fa2Address = toAddress assetFA2
        , tokenId = assetTokenId
        }
    , salePricePerToken = testSalePrice
    , tokenAmount = testTokenAmount
    }

buy :: (HasCallStack, MonadNettest caps base m) => TestData -> TAddress (MarketplaceTezEntrypoints ()) -> m ()
buy TestData{testSalePrice} contract =
  transfer TransferData
    { tdTo = contract
    , tdAmount = testSalePrice
    , tdEntrypoint = ep "buy"
    , tdParameter = SaleId 0
    }

buyAll :: (HasCallStack, MonadNettest caps base m) => TestData -> TAddress (MarketplaceTezEntrypoints ()) -> m ()
buyAll testData@TestData{testTokenAmount} contract =
  replicateM_ (fromIntegral testTokenAmount) $
    buy testData contract

cancel :: (HasCallStack, MonadNettest caps base m) => TAddress (MarketplaceTezEntrypoints ()) -> m ()
cancel contract =
  call contract (Call @"Cancel") (SaleId 0)
