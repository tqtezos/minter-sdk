module Test.Marketplace.TezOffchain where

import Data.Int ()
import Data.List ()
import qualified Data.Map as Map
import Data.Set as Set

import Hedgehog (Gen, Property, forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Morley.Nettest

import Hedgehog.Gen.Tezos.Core (genMutez')
import qualified Indigo.Contracts.FA2Sample as FA2
import Lorentz hiding (balance, contract)
import Michelson.Interpret.Pack

import Lorentz.Contracts.Marketplace.Contracts
import Lorentz.Contracts.Marketplace.Tez
import Lorentz.Contracts.Marketplace.TezOffchain

import Lorentz.Contracts.PausableAdminOption
import Lorentz.Contracts.Spec.FA2Interface (TokenId(TokenId))
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Test.Util
import Tezos.Crypto

hprop_Contracts_balance_is_zero_after_a_sale_is_concluded :: Property
hprop_Contracts_balance_is_zero_after_a_sale_is_concluded =
  property $ do
    testData <- forAll genTestData

    clevelandProp $ do
      setup@Setup{seller, assetFA2} <- testSetup testData
      contract <- originateOffchainTezMarketplaceContract setup

      withSender seller $ do
        sell testData setup contract
        offchainBuyAll testData setup contract

      contractAssetBalance <- balanceOf assetFA2 assetTokenId contract
      contractTezBalance <- getBalance contract
      contractAssetBalance @== 0
      contractTezBalance @== 0

hprop_Assets_are_transferred_to_buyer_after_confirm :: Property
hprop_Assets_are_transferred_to_buyer_after_confirm  =
  property $ do
    testData@TestData{testTokenAmount} <- forAll genTestData
    clevelandProp $ do
      setup@Setup{seller, buyer, assetFA2} <- testSetup testData
      contract <- originateOffchainTezMarketplaceContract setup

      withSender seller $ do
        sell testData setup contract
        forM_ [1 .. testTokenAmount] \n -> do
          offchainBuy buyer n contract
          confirmPurchase buyer contract
          buyerAssetBalance <- balanceOf assetFA2 assetTokenId buyer
          buyerAssetBalance @== n

      sellerAssetBalance <- balanceOf assetFA2 assetTokenId seller
      sellerAssetBalance @== 0

hprop_Assets_are_not_transferred_to_buyer_after_offchain_buy :: Property
hprop_Assets_are_not_transferred_to_buyer_after_offchain_buy  =
  property $ do
    testData@TestData{testTokenAmount} <- forAll genTestData
    clevelandProp $ do
      setup@Setup{seller, buyer, assetFA2} <- testSetup testData
      contract <- originateOffchainTezMarketplaceContract setup

      withSender seller $ do
        sell testData setup contract
        offchainBuy buyer 1 contract
        buyerAssetBalance <- balanceOf assetFA2 assetTokenId buyer
        contractAssetBalance <- balanceOf assetFA2 assetTokenId contract
        buyerAssetBalance @== 0
        contractAssetBalance @== testTokenAmount

hprop_Cant_offchain_buy_more_assets_than_are_available :: Property
hprop_Cant_offchain_buy_more_assets_than_are_available =
  property $ do
    testData@TestData{testTokenAmount} <- forAll genTestData

    clevelandProp $ do
      setup@Setup{seller, buyer} <- testSetup testData
      contract <- originateOffchainTezMarketplaceContract setup

      withSender seller $ do
        sell testData setup contract
        offchainBuyAll testData setup contract
        offchainBuy buyer (testTokenAmount + 1) contract
          & expectTransferFailure [failedWith $ constant [mt|NO_SALE|]]

hprop_Cant_buy_with_tez_after_all_assets_are_reserved_through_offchain_buy :: Property
hprop_Cant_buy_with_tez_after_all_assets_are_reserved_through_offchain_buy =
  property $ do
    testData@TestData{testTokenAmount} <- forAll genTestData

    clevelandProp $ do
      setup@Setup{seller, buyer} <- testSetup testData
      contract <- originateOffchainTezMarketplaceContract setup

      withSender seller $ do
        sell testData setup contract
        offchainBuyAll testData setup contract

      withSender buyer $ do
        buy testData contract
          & expectTransferFailure
          if testTokenAmount == 0
            then [failedWith $ constant ([mt|FA2_INSUFFICIENT_BALANCE|], 1 :: Natural, 0 :: Natural)]
            else [failedWith $ constant [mt|NO_SALE|]]

hprop_Offchain_buy_and_confirm_equals_buy :: Property
hprop_Offchain_buy_and_confirm_equals_buy =
  property $ do
    testData <- forAll genTestData

    clevelandProp $ do
      setup1@Setup{seller = seller1, buyer = buyer1, assetFA2 = assetFA2V1} <- testSetup testData
      setup2@Setup{seller = seller2, buyer = buyer2, assetFA2 = assetFA2V2} <- testSetup testData
      contract1 <- originateOffchainTezMarketplaceContract setup1
      contract2 <- originateOffchainTezMarketplaceContract setup2

      withSender seller1 $
        sell testData setup1 contract1
      withSender seller2 $
        sell testData setup2 contract2

      withSender seller1 $
        offchainBuyAll testData setup1 contract1

      withSender buyer2 $
        buyAll testData contract2

      buyerAssetBalance1 <- balanceOf assetFA2V1 assetTokenId buyer1
      contractAssetBalance1 <- balanceOf assetFA2V1 assetTokenId contract1
      sellerAssetBalance1 <- balanceOf assetFA2V1 assetTokenId seller1

      buyerAssetBalance2 <- balanceOf assetFA2V2 assetTokenId buyer2
      contractAssetBalance2 <- balanceOf assetFA2V2 assetTokenId contract2
      sellerAssetBalance2 <- balanceOf assetFA2V2 assetTokenId seller2

      marketplaceStorage1 <- toVal . marketplaceStorage <$> getStorage' contract1
      marketplaceStorage2 <- toVal . marketplaceStorage <$> getStorage' contract2

      marketplaceStorage1 @== marketplaceStorage2
      buyerAssetBalance1 @== buyerAssetBalance2
      contractAssetBalance1 @== contractAssetBalance2
      sellerAssetBalance1 @== sellerAssetBalance2

hprop_Offchain_buy_and_revoke_equals_do_nothing :: Property
hprop_Offchain_buy_and_revoke_equals_do_nothing =
  property $ do
    testData <- forAll genTestData

    clevelandProp $ do
      setup@Setup{seller, assetFA2, buyer} <- testSetup testData
      contract <- originateOffchainTezMarketplaceContract setup

      withSender seller $ do
        sell testData setup contract

      buyerAssetBalanceInit <- balanceOf assetFA2 assetTokenId buyer
      contractAssetBalanceInit <- balanceOf assetFA2 assetTokenId contract
      sellerAssetBalanceInit <- balanceOf assetFA2 assetTokenId seller
      marketplaceStorageInit <- toVal <$>
                          marketplaceStorage <$>
                          getStorage' contract

      withSender seller $ do
        offchainBuy buyer 1 contract
        revokePurchase buyer contract

      buyerAssetBalanceFinal <- balanceOf assetFA2 assetTokenId buyer
      contractAssetBalanceFinal  <- balanceOf assetFA2 assetTokenId contract
      sellerAssetBalanceFinal  <- balanceOf assetFA2 assetTokenId seller
      marketplaceStorageFinal <- toVal <$>
                                 marketplaceStorage <$>
                                 getStorage' contract

      buyerAssetBalanceInit @== buyerAssetBalanceFinal
      contractAssetBalanceInit @== contractAssetBalanceFinal
      sellerAssetBalanceInit @== sellerAssetBalanceFinal
      marketplaceStorageInit @== marketplaceStorageFinal

hprop_Forged_offchain_buy_fails :: Property
hprop_Forged_offchain_buy_fails =
  property $ do
    testData <- forAll genTestData
    clevelandProp $ do
      setup@Setup{seller, buyer} <- testSetup testData
      contract <- originateOffchainTezMarketplaceContract setup

      withSender seller $ do
        missignedBytes <- fst <$> mkPermitToForge (SaleId 0) 1 contract
        sell testData setup contract
        offchainBuyForged buyer 1 contract
          & expectTransferFailure
            [failedWith $ constant ([mt|MISSIGNED|], missignedBytes)]

hprop_Cant_offchain_buy_if_purchaser_has_pending_purchase_present :: Property
hprop_Cant_offchain_buy_if_purchaser_has_pending_purchase_present  =
  property $ do
    testData <- forAll genTestData

    clevelandProp $ do
      setup@Setup{seller, buyer} <- addSaleAndPendingPurcahseToInitialStorage testData <$> testSetup testData
      contract <- originateOffchainTezMarketplaceContract setup

      withSender seller $ do
        offchainBuy buyer 1 contract
          & expectTransferFailure
            [failedWith $ constant [mt|PENDING_PURCHASE_PRESENT|]]
  where
  addSaleAndPendingPurcahseToInitialStorage :: TestData -> Setup -> Setup
  addSaleAndPendingPurcahseToInitialStorage TestData{testSalePrice, testTokenAmount} setup@Setup{storage, buyer, seller, assetFA2} =
    setup
      { storage = storage
          { marketplaceStorage = (marketplaceStorage storage)
            { sales = BigMap $ Map.fromList
               [ (SaleId 0
                 , SaleParamTezOffchain
                   { seller = seller
                   , saleDataTez = SaleDataTez
                     { saleToken = SaleToken
                       { fa2Address = toAddress assetFA2
                       , tokenId = assetTokenId
                       }
                     , salePricePerToken = testSalePrice
                     , tokenAmount = testTokenAmount
                     }
                   , pendingPurchases = Set.singleton buyer
                   }
                 )
               ]
            }
          }
      }

hprop_Consecutive_offchain_buy_equals_iterative_buy :: Property
hprop_Consecutive_offchain_buy_equals_iterative_buy =
    property $ do
    testData <- forAll genTestData

    clevelandProp $ do
      setup1@Setup{seller} <- testSetup testData
      setup2 <- testSetup testData
      contract1 <- originateOffchainTezMarketplaceContract setup1
      contract2 <- originateOffchainTezMarketplaceContract setup2

      withSender seller $ do
        sell testData setup1 contract1
        sell testData setup2 contract2
        offchainBuyAllConsecutive testData contract1
        offchainBuyAll testData setup2 contract2

      marketplaceStorage1 <- toVal . marketplaceStorage <$> getStorage' contract1
      marketplaceStorage2 <- toVal . marketplaceStorage <$> getStorage' contract2
      marketplaceStorage1 @== marketplaceStorage2

hprop_Batch_offchain_buy_equals_consecutive_buy :: Property
hprop_Batch_offchain_buy_equals_consecutive_buy =
    property $ do
    testData <- forAll genTestData

    clevelandProp $ do
      setup1@Setup{seller} <- testSetup testData
      setup2 <- testSetup testData
      contract1 <- originateOffchainTezMarketplaceContract setup1
      contract2 <- originateOffchainTezMarketplaceContract setup2

      withSender seller $ do
        sell testData setup1 contract1
        sell testData setup2 contract2
        offchainBuyAllConsecutive testData contract1
        offchainBuyAllBatch testData contract2

      marketplaceStorage1 <- toVal . marketplaceStorage <$> getStorage' contract1
      marketplaceStorage2 <- toVal . marketplaceStorage <$> getStorage' contract2
      marketplaceStorage1 @== marketplaceStorage2

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------
assetTokenId :: TokenId
assetTokenId = TokenId 1

data TestData = TestData
  { testTokenAmount :: Natural -- ^ Amount of tokens to sell
  , testSalePrice :: Mutez -- ^ The price of the NFT / each FT being sold.
  , testBuyersBalance :: Mutez
  }
  deriving stock (Show)

genTestData :: Gen TestData
genTestData = do
  tokenAmount <- Gen.integral (Range.constant 1 10)
  salePrice <- genMutez' (Range.linear 0 10000)

  -- Make sure the buyer has enough balance to make the purchase.
  let minBuyersBalance = salePrice * fromIntegral tokenAmount
  buyersBalance <- genMutez' (Range.linear minBuyersBalance (2 * minBuyersBalance))

  pure $ TestData
    { testTokenAmount = tokenAmount
    , testSalePrice = salePrice
    , testBuyersBalance = buyersBalance
    }

data Setup = Setup
  { seller :: Address
  , buyer :: Address
  , assetFA2 :: ContractHandler FA2.FA2SampleParameter FA2.Storage
  , storage :: MarketplaceTezOffchainStorage ()
  }

testSetup :: MonadNettest caps base m => TestData -> m Setup
testSetup testData = do
  seller <- newAddress "seller"
  buyer <- newAddress "buyer"

  let storage =
        initMarketplaceTezOffchainStorage $
          Just AdminStorage
              { admin = seller
              , pendingAdmin = Nothing
              , paused = False
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

originateOffchainTezMarketplaceContract :: MonadNettest caps base m => Setup -> m $ ContractHandler (MarketplaceTezOffchainEntrypoints Never) (MarketplaceTezOffchainStorage ())
originateOffchainTezMarketplaceContract Setup{storage, seller, assetFA2} = do
  contract <- originateSimple "marketplace-tez-offchain"
    storage
    marketplaceTezOffchainContract

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

confirmPurchase :: (HasCallStack, MonadNettest caps base m) => Address -> ContractHandler (MarketplaceTezOffchainEntrypoints Never) st -> m ()
confirmPurchase buyer contract =
    call contract (Call @"Confirm_purchases")
      [
        PendingPurchase
          {
            saleId = SaleId 0
          , purchaser = buyer
          }
      ]

revokePurchase :: (HasCallStack, MonadNettest caps base m) => Address -> ContractHandler (MarketplaceTezOffchainEntrypoints Never) st -> m ()
revokePurchase buyer contract =
    call contract (Call @"Revoke_purchases")
      [
        PendingPurchase
          {
            saleId = SaleId 0
          , purchaser = buyer
          }
      ]


sell :: (HasCallStack, MonadNettest caps base m) => TestData -> Setup -> ContractHandler (MarketplaceTezOffchainEntrypoints Never) st -> m ()
sell TestData{testSalePrice, testTokenAmount} Setup{assetFA2} contract =
  call contract (Call @"Sell") SaleDataTez
    { saleToken = SaleToken
        { fa2Address = toAddress assetFA2
        , tokenId = assetTokenId
        }
    , salePricePerToken = testSalePrice
    , tokenAmount = testTokenAmount
    }

-- a single buyer buys all by alternatively offchain buying and confirming
offchainBuyAll :: (HasCallStack, MonadNettest caps base m) => TestData -> Setup -> ContractHandler (MarketplaceTezOffchainEntrypoints Never) st -> m ()
offchainBuyAll TestData{testTokenAmount} Setup{buyer} contract = do
    forM_ [1 .. testTokenAmount] \permitCounter -> do
      offchainBuy buyer permitCounter contract
      confirmPurchase buyer contract

-- N addresses buy all N assets in a sale conseutively, and then all N are confirmed
offchainBuyAllConsecutive :: (HasCallStack, MonadNettest caps base m) => TestData -> ContractHandler (MarketplaceTezOffchainEntrypoints Never) st -> m ()
offchainBuyAllConsecutive TestData{testTokenAmount} contract = do
  addresses <- mkNAddresses testTokenAmount
  forM_  (zip [1 .. testTokenAmount] addresses) ( \(counter, buyer) -> do
      offchainBuy buyer counter contract
      pure (counter + 1)
    )
  mapM_ (`confirmPurchase` contract) addresses

-- N addresses buy all N assets in a sale by submitting N permits that are submitted to the contract together in batch.
offchainBuyAllBatch :: (HasCallStack, MonadNettest caps base m) => TestData -> ContractHandler (MarketplaceTezOffchainEntrypoints Never) st -> m ()
offchainBuyAllBatch TestData{testTokenAmount} contract = do
    addresses <- mkNAddresses testTokenAmount
    offchainBuyBatch addresses contract
    mapM_ (`confirmPurchase` contract) addresses

mkNAddresses :: (HasCallStack, MonadNettest caps base m) => Natural -> m [Address]
mkNAddresses n =
    replicateM (fromIntegral n) (newAddress auto)

mkPermitToForge :: (HasCallStack, MonadNettest caps base m) => SaleId -> Natural -> ContractHandler (MarketplaceTezOffchainEntrypoints Never) st -> m (ByteString, PublicKey)
mkPermitToForge buyParam counter contract = do
  aliasAddress <- newAddress "forged"
  aliasPK <- getPublicKey aliasAddress
  unsignedPermit <- mkPermitToSign buyParam counter contract
  pure (unsignedPermit, aliasPK)

mkPermitToSign :: (HasCallStack, MonadNettest caps base m) => SaleId -> Natural -> ContractHandler (MarketplaceTezOffchainEntrypoints Never) st -> m ByteString
mkPermitToSign buyParam counter contract = do
  marketplaceChainId <- getChainId
  let unsigned = packValue' $ toVal ((marketplaceChainId, marketplaceAddress), (counter, buyParamHash))
  pure unsigned
  where buyParamHash = blake2b $ packValue' $ toVal buyParam
        marketplaceAddress = toAddress contract

offchainBuy :: (HasCallStack, MonadNettest caps base m) => Address -> Natural -> ContractHandler (MarketplaceTezOffchainEntrypoints Never) st -> m ()
offchainBuy buyer counter contract = do
  buyerPK <- getPublicKey buyer
  unsigned <- mkPermitToSign buyParam counter contract
  signature <- signBytes unsigned buyer
  call contract (Call @"Offchain_buy")
    [OffchainBuyParam
      {
        saleId = buyParam
      , permit = Permit
          {
            signerKey = buyerPK
          , signature = signature
          }
      }
    ]
  where buyParam = SaleId 0

offchainBuyBatch :: (HasCallStack, MonadNettest caps base m) => [Address] -> ContractHandler (MarketplaceTezOffchainEntrypoints Never) st -> m ()
offchainBuyBatch buyers contract = do
  let numBuyers = length buyers
  param <- forM (zip [1 .. numBuyers] buyers) $ \(counter, buyer) -> do
    buyerPK <- getPublicKey buyer
    unsigned <- mkPermitToSign buyParam (fromIntegral counter) contract
    signature <- signBytes unsigned buyer
    return OffchainBuyParam {
        saleId = buyParam
      , permit = Permit
          {
            signerKey = buyerPK
          , signature = signature
          }
      }
  call contract (Call @"Offchain_buy") param
  where buyParam = SaleId 0

offchainBuyForged :: (HasCallStack, MonadNettest caps base m) => Address -> Natural -> ContractHandler (MarketplaceTezOffchainEntrypoints Never) st -> m ByteString
offchainBuyForged buyer counter contract = do
  let buyParam = SaleId 0
  (unsigned, forgedPK) <- mkPermitToForge buyParam counter contract
  signature <- signBytes unsigned buyer
  (\() -> unsigned) <$> call contract (Call @"Offchain_buy")
    [OffchainBuyParam
      {
        saleId = buyParam
      , permit = Permit
          {
            signerKey = forgedPK
          , signature = signature
          }
      }
    ]

buy :: (HasCallStack, MonadNettest caps base m) => TestData -> ContractHandler (MarketplaceTezOffchainEntrypoints Never) st -> m ()
buy TestData{testSalePrice} contract =
  transfer TransferData
    { tdTo = contract
    , tdAmount = testSalePrice
    , tdEntrypoint = ep "buy"
    , tdParameter = SaleId 0
    }

buyAll :: (HasCallStack, MonadNettest caps base m) => TestData -> ContractHandler (MarketplaceTezOffchainEntrypoints Never) st -> m ()
buyAll testData@TestData{testTokenAmount} contract =
  replicateM_ (fromIntegral testTokenAmount) $
    buy testData contract
