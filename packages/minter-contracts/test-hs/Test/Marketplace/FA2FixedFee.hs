module Test.Marketplace.FA2FixedFee where

import qualified Data.Map as Map
import Hedgehog (Gen, Property, forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Morley.Nettest

import Lorentz hiding (balance, contract)
import Michelson.Typed (convertContract, untypeValue)

import qualified Indigo.Contracts.FA2Sample as FA2
import Lorentz.Contracts.Marketplace.FA2FixedFee
import Lorentz.Contracts.PausableAdminOption
import Lorentz.Contracts.Spec.FA2Interface (TokenId(TokenId))
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Test.Util

hprop_Every_sale_sends_a_fee_to_the_fee_collector :: Property
hprop_Every_sale_sends_a_fee_to_the_fee_collector =
  property $ do
    testData@TestData{testSalePrice, testFeePercent} <- forAll genTestData
    clevelandProp $ do
      setup@Setup{seller, buyer, feeCollector, moneyFA2} <- testSetup testData
      contract <- originateMarketplaceContract setup

      withSender seller $
        sell testData setup contract
      withSender buyer $
        buy contract

      let expectedFee = testSalePrice * testFeePercent `div` 100
      feeCollectorsBalance <- balanceOf moneyFA2 moneyTokenId feeCollector
      feeCollectorsBalance @== expectedFee

hprop_Tokens_are_transferred_to_seller :: Property
hprop_Tokens_are_transferred_to_seller  =
  property $ do
    testData@TestData{testSalePrice, testFeePercent} <- forAll genTestData

    clevelandProp $ do
      setup@Setup{seller, buyer, moneyFA2} <- testSetup testData
      contract <- originateMarketplaceContract setup

      withSender seller $
        sell testData setup contract
      withSender buyer $
        buy contract

      let expectedFee = testSalePrice * testFeePercent `div` 100
      let expectedSellerBalance = testSalePrice - expectedFee
      sellerBalance <- balanceOf moneyFA2 moneyTokenId seller
      sellerBalance @== expectedSellerBalance

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
        buy contract

      sellerNftBalance <- balanceOf nftFA2 nftTokenId seller
      sellerNftBalance @== 0
      buyerNftBalance <- balanceOf nftFA2 nftTokenId buyer
      buyerNftBalance @== 1

hprop_Contracts_balance_is_zero_after_a_sale_is_concluded :: Property
hprop_Contracts_balance_is_zero_after_a_sale_is_concluded =
  property $ do
    testData <- forAll genTestData

    clevelandProp $ do
      setup@Setup{seller, buyer, nftFA2, moneyFA2} <- testSetup testData
      contract <- originateMarketplaceContract setup

      withSender seller $
        sell testData setup contract
      withSender buyer $
        buy contract

      contractTokenBalance <- balanceOf moneyFA2 moneyTokenId contract
      contractNftBalance <- balanceOf nftFA2 nftTokenId contract
      contractTezBalance <- getBalance contract
      contractTokenBalance @== 0
      contractNftBalance @== 0
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
        buy contract

      finalGlobalBalance <- sum <$> forM [feeCollector, buyer, seller] \account ->
        balanceOf moneyFA2 moneyTokenId account

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
        sell testData setup contract `expectFailure` failedWith contract [mt|FEE_TOO_HIGH|]

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
        buy contract `expectFailure` failedWith contract [mt|FEE_TOO_HIGH|]

  where
    addSaleToInitialStorage :: TestData -> Setup -> Setup
    addSaleToInitialStorage TestData{testSalePrice} setup@Setup{storage, seller, nftFA2, moneyFA2} =
      setup
        { storage = storage
            { sales = BigMap $ Map.fromList
                [ ( SaleId 0
                  , SaleParam
                    { seller = seller
                    , saleData = SaleData
                      { salePricePerToken = testSalePrice
                      , saleToken = SaleToken
                        { fa2Address = toAddress nftFA2
                        , tokenId = nftTokenId
                        }
                      , moneyToken = MoneyToken
                        { fa2Address = toAddress moneyFA2
                        , tokenId = moneyTokenId
                        }
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

      storage <- fromVal @MarketplaceStorage <$> getStorage' contract
      sales storage @== mempty

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

nftTokenId, moneyTokenId :: TokenId
nftTokenId = TokenId 1
moneyTokenId = TokenId 2

data TestData = TestData
  { testFeePercent :: Natural
  , testSalePrice :: Natural
  , testBuyersBalance :: Natural
  }
  deriving stock (Show)

genTestData :: Gen TestData
genTestData = do
  feePercent <- Gen.integral (Range.linear 0 100)
  salePrice <- Gen.integral (Range.linear 0 10000)

  -- Make sure the buyer has enough balance to make the purchase.
  buyersBalance <- Gen.integral (Range.linear salePrice (2 * salePrice))

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
  , moneyFA2 :: TAddress FA2.FA2SampleParameter
  , storage :: MarketplaceStorage
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
          , fee = FeeData
            { feeAddress = feeCollector
            , feePercent = testFeePercent testData
            }
          }

  -- Create two FA2 contracts, and give an NFT to the seller and some tokens to the buyer.
  nftFA2 <- originateSimple "nft_fa2"
    FA2.Storage
    { sLedger = BigMap $ Map.fromList [((seller, nftTokenId), 1)]
    , sOperators = mempty
    , sTokenMetadata = mempty
    }
    (FA2.fa2Contract def { FA2.cAllowedTokenIds = [nftTokenId] })
  moneyFA2 <- originateSimple "money_fa2"
    FA2.Storage
    { sLedger = BigMap $ Map.fromList [((buyer, moneyTokenId), testBuyersBalance testData)]
    , sOperators = mempty
    , sTokenMetadata = mempty
    }
    (FA2.fa2Contract def { FA2.cAllowedTokenIds = [moneyTokenId] })
  pure Setup {..}

originateMarketplaceContract :: MonadNettest caps base m => Setup -> m (TAddress MarketplaceEntrypoints)
originateMarketplaceContract Setup{storage, seller, buyer, nftFA2, moneyFA2} = do
  contract <- TAddress @MarketplaceEntrypoints <$> originateUntypedSimple "marketplace-fa2-fixed-fee"
    (untypeValue $ toVal storage)
    (convertContract marketplaceFixedFeeContract)

  -- Make the contract an operator for the seller and the buyer.
  withSender seller $ do
    call nftFA2 (Call @"Update_operators")
      [ FA2.AddOperator FA2.OperatorParam
          { opOwner = seller
          , opOperator = toAddress contract
          , opTokenId = nftTokenId
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

sell :: (HasCallStack, MonadNettest caps base m) => TestData -> Setup -> TAddress MarketplaceEntrypoints -> m ()
sell TestData{testSalePrice} Setup{nftFA2, moneyFA2} contract =
  call contract (Call @"Sell") SaleData
    { salePricePerToken = testSalePrice
    , saleToken = SaleToken
      { fa2Address = toAddress nftFA2
      , tokenId = nftTokenId
      }
    , moneyToken = MoneyToken
      { fa2Address = toAddress moneyFA2
      , tokenId = moneyTokenId
      }
    , tokenAmount = 1
    }

buy :: (HasCallStack, MonadNettest caps base m) => TAddress MarketplaceEntrypoints -> m ()
buy contract =
  call contract (Call @"Buy") (SaleId 0)

cancel :: (HasCallStack, MonadNettest caps base m) => TAddress MarketplaceEntrypoints -> m ()
cancel contract =
  call contract (Call @"Cancel") (SaleId 0)
