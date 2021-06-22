module Test.Marketplace.TezOffchain where

import qualified Data.Map as Map
import Hedgehog (Gen, Property, forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Morley.Nettest

import Lorentz hiding (balance, contract)
import Michelson.Typed (convertContract, untypeValue)
import Michelson.Interpret.Pack

import qualified Indigo.Contracts.FA2Sample as FA2
import Lorentz.Contracts.Marketplace.TezOffchain
import Lorentz.Contracts.PausableAdminOption
import Lorentz.Contracts.Spec.FA2Interface (TokenId(TokenId))
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Test.Util
import Tezos.Crypto

hprop_Contracts_balance_is_zero_after_a_sale_is_concluded :: Property
hprop_Contracts_balance_is_zero_after_a_sale_is_concluded =
  property $ do
    testData@TestData{testTokenAmount} <- forAll genTestData

    clevelandProp $ do
      setup@Setup{seller, assetFA2} <- testSetup testData
      contract <- originateOffchainTezMarketplaceContract setup

      withSender seller $
        sell testData setup contract
      withSender seller $ 
          replicateM_ (fromIntegral testTokenAmount) $ do
            marketplaceStorage <- fromVal @(MarketplaceTezOffchainStorage ()) <$> getStorage' contract
            let permitCounter = counter marketplaceStorage 
            offchainBuy setup permitCounter contract
            confirmPurchase setup contract

      contractAssetBalance <- balanceOf assetFA2 assetTokenId contract
      contractTezBalance <- getBalance contract
      contractAssetBalance @== 0
      contractTezBalance @== 0

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
  tokenAmount <- Gen.integral (Range.constant 0 10)
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

confirmPurchase :: (HasCallStack, MonadNettest caps base m) => Setup -> TAddress (MarketplaceTezOffchainEntrypoints ()) -> m ()
confirmPurchase Setup{buyer} contract = 
    call contract (Call @"Confirm_purchases") 
      [
        PendingPurchase 
          {
            saleId = SaleId 0
          , purchaser = buyer
          }
      ]

revokePurchase :: (HasCallStack, MonadNettest caps base m) => Setup -> TAddress (MarketplaceTezOffchainEntrypoints ()) -> m ()
revokePurchase Setup{buyer} contract = 
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

offchainBuy :: (HasCallStack, MonadNettest caps base m) => Setup -> Natural -> TAddress (MarketplaceTezOffchainEntrypoints ()) -> m ()
offchainBuy Setup{buyer} counter contract = do
  signerPK <- getPublicKey buyer
  marketplaceChainId <- getChainId
  let unsigned = packValue' $ toVal ((marketplaceChainId, marketplaceAddress), (counter, buyParamHash))
  signature <- signBinary unsigned buyer 
  call contract (Call @"Offchain_buy") 
    [OffchainBuyParam
      {
        saleId = buyParam
      , permit = Permit
          {
            signerKey = signerPK
          , signature = unTSignature signature
          } 
      }
    ]
  where buyParam = SaleId 0 
        buyParamHash = blake2b $ packValue' $ toVal buyParam 
        marketplaceAddress = toAddress contract