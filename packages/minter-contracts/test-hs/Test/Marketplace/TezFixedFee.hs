module Test.Marketplace.TezFixedFee where

import qualified Data.Map as Map
import Hedgehog (Gen, Property, forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Morley.Nettest

import Lorentz hiding (balance, contract)
import Michelson.Typed (convertContract, untypeValue)
import Tezos.Core (unMutez, unsafeMkMutez)

import qualified Indigo.Contracts.FA2Sample as FA2
import Lorentz.Contracts.Marketplace.TezFixedFee
import Lorentz.Contracts.PausableAdminOption
import Lorentz.Contracts.Spec.FA2Interface (TokenId(TokenId))
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Test.Util

hprop_Every_sale_sends_a_fee_to_the_fee_collector :: Property
hprop_Every_sale_sends_a_fee_to_the_fee_collector =
  property $ do
    testData@TestData{testSalePrice, testFeePercent} <- forAll genTestData
    clevelandProp $ do
      setup@Setup{seller, buyer, feeCollector} <- testSetup testData
      contract <- originateMarketplaceContract setup
      feeCollectorInitialBalance <- getBalance feeCollector

      withSender seller $
        sell testData setup contract
      withSender buyer $
        buy testData contract

      let expectedFee = unsafeMkMutez $ ceiling @Double @Word64 ((fromIntegral (unMutez testSalePrice) * fromIntegral testFeePercent) / 100)
      feeCollectorFinalBalance <- getBalance feeCollector
      feeCollectorFinalBalance @== feeCollectorInitialBalance + expectedFee

hprop_Tez_is_transferred_to_seller :: Property
hprop_Tez_is_transferred_to_seller  =
  property $ do
    testData@TestData{testSalePrice, testFeePercent} <- forAll genTestData

    clevelandProp $ do
      setup@Setup{seller, buyer} <- testSetup testData
      contract <- originateMarketplaceContract setup
      sellerInitialBalance <- getBalance seller

      withSender seller $
        sell testData setup contract
      withSender buyer $
        buy testData contract

      let expectedFee = unsafeMkMutez $ ceiling @Double @Word64 ((fromIntegral (unMutez testSalePrice) * fromIntegral testFeePercent) / 100)
      let expectedTransfer = testSalePrice - expectedFee
      sellerFinalBalance <- getBalance seller
      sellerFinalBalance @== sellerInitialBalance + expectedTransfer

hprop_NFT_is_transferred_to_buyer :: Property
hprop_NFT_is_transferred_to_buyer  =
  property $ do
    testData <- forAll genTestData
    clevelandProp $ do
      setup@Setup{seller, buyer, nftFA2} <- testSetup testData
      contract <- originateMarketplaceContract setup

      withSender seller $
        sell testData setup contract
      withSender buyer $
        buy testData contract

      sellerNftBalance <- balanceOf nftFA2 nftTokenId seller
      sellerNftBalance @== 0
      buyerNftBalance <- balanceOf nftFA2 nftTokenId buyer
      buyerNftBalance @== 1

hprop_Contracts_balance_is_zero_after_a_sale_is_concluded :: Property
hprop_Contracts_balance_is_zero_after_a_sale_is_concluded =
  property $ do
    testData <- forAll genTestData

    clevelandProp $ do
      setup@Setup{seller, buyer, nftFA2} <- testSetup testData
      contract <- originateMarketplaceContract setup

      withSender seller $
        sell testData setup contract
      withSender buyer $
        buy testData contract

      contractNftBalance <- balanceOf nftFA2 nftTokenId contract
      contractTezBalance <- getBalance contract
      contractNftBalance @== 0
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
        buy testData contract

      finalGlobalBalance <- sum <$> forM [feeCollector, buyer, seller] \account ->
        getBalance account

      finalGlobalBalance @== initialGlobalBalance

hprop_NFT_is_held_in_escrow :: Property
hprop_NFT_is_held_in_escrow =
  property $ do
    testData <- forAll genTestData

    clevelandProp $ do
      setup@Setup{seller, nftFA2} <- testSetup testData
      contract <- originateMarketplaceContract setup

      withSender seller $
        sell testData setup contract

      contractNftBalance <- balanceOf nftFA2 nftTokenId contract
      contractNftBalance @== 1

      sellerNftBalance <- balanceOf nftFA2 nftTokenId seller
      sellerNftBalance @== 0

hprop_Cant_sell_if_fee_is_too_high :: Property
hprop_Cant_sell_if_fee_is_too_high  =
  property $ do
    invalidFeePercent <- forAll $ Gen.integral (Range.linear 101 10000)
    testData <- forAll genTestData <&> \testData -> testData { testFeePercent = invalidFeePercent }

    clevelandProp $ do
      setup@Setup{seller} <- testSetup testData
      contract <- originateMarketplaceContract setup

      withSender seller $ do
        sell testData setup contract `expectFailure` failedWith contract [mt|FEE_PERCENT_TOO_HIGH|]

hprop_Cant_buy_if_fee_is_too_high :: Property
hprop_Cant_buy_if_fee_is_too_high  =
  property $ do
    invalidFeePercent <- forAll $ Gen.integral (Range.linear 101 10000)
    testData <- forAll genTestData <&> \testData -> testData { testFeePercent = invalidFeePercent }

    clevelandProp $ do
      -- Originate the contract with a "sale" entry in the initial storage
      setup@Setup{seller, buyer, nftFA2} <- addSaleToInitialStorage testData <$> testSetup testData
      contract <- originateMarketplaceContract setup

      -- Transfer the NFT to the contract
      withSender seller $ do
        call nftFA2 (Call @"Transfer")
          [ FA2.TransferItem
              { tiFrom = seller
              , tiTxs = one FA2.TransferDestination
                  { tdTo = toAddress contract
                  , tdTokenId = nftTokenId
                  , tdAmount = 1
                  }
              }
          ]

      -- Attempt to buy the NFT held in the contract
      withSender buyer $ do
        -- buy testData contract `expectFailure` failedWith contract [mt|FEE_PERCENT_TOO_HIGH|]
        buyResult <- attempt @SomeException $ buy testData contract
        case buyResult of
          Left _ -> pass
          Right _ -> failure "Expected `buy` to fail, but it succeeded."
  where
    addSaleToInitialStorage :: TestData -> Setup -> Setup
    addSaleToInitialStorage TestData{testSalePrice} setup@Setup{storage, seller, nftFA2} =
      setup
        { storage = storage
            { sales = BigMap $ Map.fromList
                [ (SaleId 0
                  , SaleParamTez
                    { seller = seller
                    , saleDataTez = SaleDataTez
                      { saleToken = SaleToken
                        { fa2Address = toAddress nftFA2
                        , tokenId = nftTokenId
                        }
                      , salePricePerToken = testSalePrice
                      , tokenAmount = 1
                      }
                    }
                  )
                ]
            }
        }

hprop_Cancelling_a_sale_returns_NFT_back_to_seller :: Property
hprop_Cancelling_a_sale_returns_NFT_back_to_seller =
  property $ do
    testData <- forAll genTestData
    clevelandProp $ do
      setup@Setup{seller, nftFA2} <- testSetup testData
      contract <- originateMarketplaceContract setup

      withSender seller $ do
        sell testData setup contract
        cancel contract

      contractNftBalance <- balanceOf nftFA2 nftTokenId contract
      contractNftBalance @== 0

      sellerNftBalance <- balanceOf nftFA2 nftTokenId seller
      sellerNftBalance @== 1

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

      storage <- fromVal @MarketplaceTezStorage <$> getStorage' contract
      sales storage @== mempty

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

nftTokenId :: TokenId
nftTokenId = TokenId 1

data TestData = TestData
  { testFeePercent :: Natural
  , testSalePrice :: Mutez
  , testBuyersBalance :: Mutez
  }
  deriving stock (Show)

genTestData :: Gen TestData
genTestData = do
  feePercent <- Gen.integral (Range.linear 0 100)
  salePrice <- toMutez <$> Gen.integral (Range.linear 0 10000)

  -- Make sure the buyer has enough balance to make the purchase.
  buyersBalance <- unsafeMkMutez <$> Gen.word64 (Range.linear (unMutez salePrice) (2 * unMutez salePrice))

  pure $ TestData
    { testFeePercent = feePercent
    , testSalePrice = salePrice
    , testBuyersBalance = buyersBalance
    }

data Setup = Setup
  { seller :: Address
  , buyer :: Address
  , feeCollector :: Address
  , nftFA2 :: TAddress FA2.FA2SampleParameter
  , storage :: MarketplaceTezStorage
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
          , fee = FeeData
            { feeAddress = feeCollector
            , feePercent = testFeePercent testData
            }
          }

  -- Create an FA2 contracts, and give an NFT to the seller.
  nftFA2 <- originateSimple "nft_fa2"
    FA2.Storage
    { sLedger = BigMap $ Map.fromList [((seller, nftTokenId), 1)]
    , sOperators = mempty
    , sTokenMetadata = mempty
    }
    (FA2.fa2Contract def { FA2.cAllowedTokenIds = [nftTokenId] })

  pure Setup {..}

originateMarketplaceContract :: MonadNettest caps base m => Setup -> m (TAddress MarketplaceTezEntrypoints)
originateMarketplaceContract Setup{storage, seller, nftFA2} = do
  contract <- TAddress @MarketplaceTezEntrypoints <$> originateUntypedSimple "marketplace-tez-fixed-fee"
    (untypeValue $ toVal storage)
    (convertContract marketplaceTezFixedFeeContract)

  -- Make the contract an operator for the seller.
  withSender seller $ do
    call nftFA2 (Call @"Update_operators")
      [ FA2.AddOperator FA2.OperatorParam
          { opOwner = seller
          , opOperator = toAddress contract
          , opTokenId = nftTokenId
          }
      ]

  pure contract

----------------------------------------------------------------------------
-- Call entrypoints
----------------------------------------------------------------------------

sell :: (HasCallStack, MonadNettest caps base m) => TestData -> Setup -> TAddress MarketplaceTezEntrypoints -> m ()
sell TestData{testSalePrice} Setup{nftFA2} contract =
  call contract (Call @"Sell") SaleDataTez
    { saleToken = SaleToken
        { fa2Address = toAddress nftFA2
        , tokenId = nftTokenId
        }
    , salePricePerToken = testSalePrice
    , tokenAmount = 1
    }

buy :: (HasCallStack, MonadNettest caps base m) => TestData -> TAddress MarketplaceTezEntrypoints -> m ()
buy TestData{testSalePrice} contract =
  transfer TransferData
    { tdTo = contract
    , tdAmount = testSalePrice
    , tdEntrypoint = ep "buy"
    , tdParameter = SaleId 0
    }

cancel :: (HasCallStack, MonadNettest caps base m) => TAddress MarketplaceTezEntrypoints -> m ()
cancel contract =
  call contract (Call @"Cancel") (SaleId 0)
