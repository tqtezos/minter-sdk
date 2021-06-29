module Test.Marketplace.TezOffchain where

import qualified Data.Map as Map
import Data.List as List
import Data.Set as Set 
import Data.Int

import Hedgehog (Gen, Property, forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Morley.Nettest

import Lorentz hiding (balance, contract)
import Michelson.Typed (convertContract, untypeValue)
import Michelson.Interpret.Pack

import qualified Indigo.Contracts.FA2Sample as FA2

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
      setup@Setup{seller, buyer, assetFA2} <- testSetup testData
      contract <- originateOffchainTezMarketplaceContract setup

      withSender seller $ do
        sell testData setup contract
        offchainBuyAndConfirmAll buyer testData contract

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
        offchainBuyAll testData contract
        offchainBuy buyer (testTokenAmount + 1) contract `expectFailure`
          failedWith contract [mt|NO_SALE|]

hprop_Cant_buy_with_tez_after_all_assets_are_reserved_through_offchain_buy :: Property
hprop_Cant_buy_with_tez_after_all_assets_are_reserved_through_offchain_buy =
  property $ do
    testData@TestData{testTokenAmount} <- forAll genTestData

    clevelandProp $ do
      setup@Setup{seller, assetFA2, buyer} <- testSetup testData
      contract <- originateOffchainTezMarketplaceContract setup

      withSender seller $ do
        sell testData setup contract
        offchainBuyAll testData contract
      
      withSender buyer $ do 
        buy testData contract `expectFailure`
          if testTokenAmount == 0
            then failedWith assetFA2 ([mt|FA2_INSUFFICIENT_BALANCE|], 1 :: Natural, 0 :: Natural)
            else failedWith contract [mt|NO_SALE|]

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
        offchainBuyAndConfirmAll buyer1 testData contract1

      withSender buyer2 $ 
        buyAll testData contract2
      
      buyerAssetBalance1 <- balanceOf assetFA2V1 assetTokenId buyer1
      contractAssetBalance1 <- balanceOf assetFA2V1 assetTokenId contract1
      sellerAssetBalance1 <- balanceOf assetFA2V1 assetTokenId seller1
      
      buyerAssetBalance2 <- balanceOf assetFA2V2 assetTokenId buyer2
      contractAssetBalance2 <- balanceOf assetFA2V2 assetTokenId contract2
      sellerAssetBalance2 <- balanceOf assetFA2V2 assetTokenId seller2

      marketplaceStorage1 <- toVal <$>
                             marketplaceStorage <$>
                             fromVal @(MarketplaceTezOffchainStorage ()) <$> 
                             getStorage' contract1
      marketplaceStorage2 <- toVal <$>
                             marketplaceStorage <$>
                             fromVal @(MarketplaceTezOffchainStorage ()) <$> 
                             getStorage' contract2
                             
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
                          fromVal @(MarketplaceTezOffchainStorage ()) <$> 
                          getStorage' contract

      withSender seller $ do
        offchainBuy buyer 1 contract
        revokePurchase buyer contract 

      buyerAssetBalanceFinal <- balanceOf assetFA2 assetTokenId buyer
      contractAssetBalanceFinal  <- balanceOf assetFA2 assetTokenId contract
      sellerAssetBalanceFinal  <- balanceOf assetFA2 assetTokenId seller
      marketplaceStorageFinal <- toVal <$>
                                 marketplaceStorage <$>
                                 fromVal @(MarketplaceTezOffchainStorage ()) <$> 
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
        offchainBuyForged buyer 1 contract `expectFailure` failedWith contract 
          ([mt|MISSIGNED|], missignedBytes)

hprop_Cant_offchain_buy_if_purchaser_has_pending_purchase_present :: Property
hprop_Cant_offchain_buy_if_purchaser_has_pending_purchase_present  =
  property $ do
    testData <- forAll genTestData

    clevelandProp $ do
      setup@Setup{seller, buyer} <- addSaleAndPendingPurcahseToInitialStorage testData <$> testSetup testData
      contract <- originateOffchainTezMarketplaceContract setup

      withSender seller $ do
        offchainBuy buyer 1 contract `expectFailure`
          failedWith contract [mt|PENDING_PURCHASE_PRESENT|]
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

hprop_Batch_offchain_buy_equals_iterative_buy :: Property
hprop_Batch_offchain_buy_equals_iterative_buy =
    property $ do
    testData@TestData{testTokenAmount} <- forAll genTestData

    clevelandProp $ do
      setup1@Setup{seller, assetFA2, buyer} <- testSetup testData
      setup2@Setup{seller, assetFA2, buyer} <- testSetup testData
      contract1 <- originateOffchainTezMarketplaceContract setup1
      contract2 <- originateOffchainTezMarketplaceContract setup2

      withSender seller $ do
        sell testData setup1 contract1
        sell testData setup2 contract2
        offchainBuyAll testData contract1
        offchainBuyAllBatchAndConfirm testData contract2
    
      marketplaceStorage1 <- toVal <$>
                             marketplaceStorage <$>
                             fromVal @(MarketplaceTezOffchainStorage ()) <$> 
                             getStorage' contract1
      marketplaceStorage2 <- toVal <$>
                             marketplaceStorage <$>
                             fromVal @(MarketplaceTezOffchainStorage ()) <$> 
                             getStorage' contract2
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
  , assetFA2 :: TAddress FA2.FA2SampleParameter
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

originateOffchainTezMarketplaceContract :: MonadNettest caps base m => Setup -> m (TAddress $ MarketplaceTezOffchainEntrypoints ())
originateOffchainTezMarketplaceContract Setup{storage, seller, assetFA2} = do
  contract <- TAddress @(MarketplaceTezOffchainEntrypoints ()) <$> originateUntypedSimple "marketplace-tez-offchain"
    (untypeValue $ toVal storage)
    (convertContract marketplaceTezOffchainContract)

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

confirmPurchase :: (HasCallStack, MonadNettest caps base m) => Address -> TAddress (MarketplaceTezOffchainEntrypoints ()) -> m ()
confirmPurchase buyer contract = 
    call contract (Call @"Confirm_purchases") 
      [
        PendingPurchase 
          {
            saleId = SaleId 0
          , purchaser = buyer
          }
      ]

revokePurchase :: (HasCallStack, MonadNettest caps base m) => Address -> TAddress (MarketplaceTezOffchainEntrypoints ()) -> m ()
revokePurchase buyer contract = 
    call contract (Call @"Revoke_purchases") 
      [
        PendingPurchase 
          {
            saleId = SaleId 0
          , purchaser = buyer
          }
      ]
    

sell :: (HasCallStack, MonadNettest caps base m) => TestData -> Setup -> TAddress (MarketplaceTezOffchainEntrypoints ()) -> m ()
sell TestData{testSalePrice, testTokenAmount} Setup{assetFA2} contract =
  call contract (Call @"Sell") SaleDataTez
    { saleToken = SaleToken
        { fa2Address = toAddress assetFA2
        , tokenId = assetTokenId
        }
    , salePricePerToken = testSalePrice
    , tokenAmount = testTokenAmount
    }

offchainBuyAndConfirmAll :: (HasCallStack, MonadNettest caps base m) => Address -> TestData -> TAddress (MarketplaceTezOffchainEntrypoints ()) -> m ()
offchainBuyAndConfirmAll buyer TestData{testTokenAmount} contract = do
    forM_ [1 .. testTokenAmount] \permitCounter -> do
      offchainBuy buyer permitCounter contract
      confirmPurchase buyer contract

offchainBuyAll :: (HasCallStack, MonadNettest caps base m) => TestData -> TAddress (MarketplaceTezOffchainEntrypoints ()) -> m ()
offchainBuyAll TestData{testTokenAmount} contract = 
    forM_ [1 .. testTokenAmount] \permitCounter -> do
      buyer <- newAddress "buyer"
      offchainBuy buyer permitCounter contract
      confirmPurchase buyer contract

offchainBuyAllBatchAndConfirm :: (HasCallStack, MonadNettest caps base m) => TestData -> TAddress (MarketplaceTezOffchainEntrypoints ()) -> m ()
offchainBuyAllBatchAndConfirm TestData{testTokenAmount} contract = do
    addresses <- mkNAddresses testTokenAmount 
    offchainBuyBatch addresses contract
    mapM_ (`confirmPurchase` contract) addresses

mkNAddresses :: (HasCallStack, MonadNettest caps base m) => Natural -> m [Address]
mkNAddresses n = do 
    replicateM (fromIntegral n) (newAddress "buyer")

mkPermitToForge :: (HasCallStack, MonadNettest caps base m) => SaleId -> Natural -> TAddress (MarketplaceTezOffchainEntrypoints ()) -> m (ByteString, PublicKey)
mkPermitToForge buyParam counter contract = do 
  aliasAddress <- newAddress "forged"
  aliasPK <- getPublicKey aliasAddress
  unsignedPermit <- mkPermitToSign buyParam counter contract 
  pure (unsignedPermit, aliasPK)

mkPermitToSign :: (HasCallStack, MonadNettest caps base m) => SaleId -> Natural -> TAddress (MarketplaceTezOffchainEntrypoints ()) -> m ByteString
mkPermitToSign buyParam counter contract = do 
  marketplaceChainId <- getChainId
  let unsigned = packValue' $ toVal ((marketplaceChainId, marketplaceAddress), (counter, buyParamHash))
  pure unsigned
  where buyParamHash = blake2b $ packValue' $ toVal buyParam 
        marketplaceAddress = toAddress contract

offchainBuy :: (HasCallStack, MonadNettest caps base m) => Address -> Natural -> TAddress (MarketplaceTezOffchainEntrypoints ()) -> m ()
offchainBuy buyer counter contract = do
  buyerPK <- getPublicKey buyer
  unsigned <- mkPermitToSign buyParam counter contract
  signature <- signBinary unsigned buyer 
  call contract (Call @"Offchain_buy") 
    [OffchainBuyParam
      {
        saleId = buyParam
      , permit = Permit
          {
            signerKey = buyerPK
          , signature = unTSignature signature
          } 
      }
    ]
  where buyParam = SaleId 0 

offchainBuyBatch :: (HasCallStack, MonadNettest caps base m) => [Address] -> TAddress (MarketplaceTezOffchainEntrypoints ()) -> m ()
offchainBuyBatch buyers contract = do
  (param, _) <- foldM ( curry $ \((batchParam, counter), buyer) -> do
      buyerPK <- getPublicKey buyer
      unsigned <- mkPermitToSign buyParam counter contract
      signature <- signBinary unsigned buyer 
      let newParam = OffchainBuyParam {
            saleId = buyParam
          , permit = Permit
              {
                signerKey = buyerPK
              , signature = unTSignature signature
              } 
          }
      pure ( newParam : batchParam, counter + 1) 
      ) ([], 1) buyers 
  call contract (Call @"Offchain_buy") param 
  where buyParam = SaleId 0 

offchainBuyForged :: (HasCallStack, MonadNettest caps base m) => Address -> Natural -> TAddress (MarketplaceTezOffchainEntrypoints ()) -> m ByteString
offchainBuyForged buyer counter contract = do
  let buyParam = SaleId 0 
  (unsigned, forgedPK) <- mkPermitToForge buyParam counter contract
  signature <- signBinary unsigned buyer 
  (\() -> unsigned) <$> call contract (Call @"Offchain_buy") 
    [OffchainBuyParam
      {
        saleId = buyParam
      , permit = Permit
          {
            signerKey = forgedPK
          , signature = unTSignature signature
          } 
      }
    ] 
  
buy :: (HasCallStack, MonadNettest caps base m) => TestData -> TAddress (MarketplaceTezOffchainEntrypoints ()) -> m ()
buy TestData{testSalePrice} contract =
  transfer TransferData
    { tdTo = contract
    , tdAmount = testSalePrice
    , tdEntrypoint = ep "buy"
    , tdParameter = SaleId 0
    }

buyAll :: (HasCallStack, MonadNettest caps base m) => TestData -> TAddress (MarketplaceTezOffchainEntrypoints ()) -> m ()
buyAll testData@TestData{testTokenAmount} contract =
  replicateM_ (fromIntegral testTokenAmount) $
    buy testData contract