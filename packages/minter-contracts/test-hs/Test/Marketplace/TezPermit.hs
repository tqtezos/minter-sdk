module Test.Marketplace.TezPermit where

import qualified Data.Map as Map
import Hedgehog (Gen, Property, forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Morley.Nettest

import Lorentz hiding (balance, contract)
import Michelson.Typed (convertContract, untypeValue)
import Michelson.Interpret.Pack
import Tezos.Core (unMutez, unsafeMkMutez)
import Crypto.Random (MonadRandom)

import qualified Indigo.Contracts.FA2Sample as FA2
import Lorentz.Contracts.Marketplace.TezPermit
import Lorentz.Contracts.PausableAdminOption
import Lorentz.Contracts.Spec.FA2Interface (TokenId(TokenId))
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Test.Util
import Tezos.Crypto

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
  , storage :: MarketplaceTezPermitStorage ()
  }

testSetup :: MonadNettest caps base m => TestData -> m Setup
testSetup testData = do
  seller <- newAddress "seller"
  buyer <- newAddress "buyer"

  let storage =
        initMarketplaceTezPermitStorage $
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

originateOffchainMarketplaceContract :: MonadNettest caps base m => Setup -> m (TAddress $ MarketplaceTezPermitEntrypoints ())
originateOffchainMarketplaceContract Setup{storage, seller, assetFA2} = do
  contract <- TAddress @(MarketplaceTezPermitEntrypoints ()) <$> originateUntypedSimple "marketplace-tez-offchain"
    (untypeValue $ toVal storage)
    (convertContract marketplaceTezPermitContract)

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

confirmPurchase :: (HasCallStack, MonadNettest caps base m) => Setup -> TAddress (MarketplaceTezPermitEntrypoints ()) -> m ()
confirmPurchase Setup{seller} contract = 
    call contract (Call @"Confirm_purchases") 
      [
        PendingPurchase 
          {
            saleId = SaleId 0
          , purchaser = seller
          }
      ]
    

sell :: (HasCallStack, MonadNettest caps base m) => TestData -> Setup -> TAddress (MarketplaceTezPermitEntrypoints ()) -> m ()
sell TestData{testSalePrice, testTokenAmount} Setup{assetFA2} contract =
  call contract (Call @"Sell") SaleDataTez
    { saleToken = SaleToken
        { fa2Address = toAddress assetFA2
        , tokenId = assetTokenId
        }
    , salePricePerToken = testSalePrice
    , tokenAmount = testTokenAmount
    }

offchainBuy :: (HasCallStack, MonadNettest caps base m, MonadRandom m) => TAddress (MarketplaceTezPermitEntrypoints ()) -> m ()
offchainBuy contract =
  let buyParam = SaleId 0 
      buyParamHash = packValue' $ toVal buyParam 
      secretKey = detSecretKey "edsk2rKA8YEExg9Zo2qNPiQnnYheF1DhqjLVmfKdxiFfu5GyGRZRnb" 
      pubKey = toPublic secretKey in 
  do
  signature <- sign secretKey buyParamHash
  (call contract (Call @"Offchain_buy") 
    [PermitBuyParam
      {
        saleId = buyParam
      , permit = Permit
          {
            signerKey = pubKey
          , signature = signature
          } 
      }
    ])

buyAllOffchain :: (HasCallStack, MonadNettest caps base m, MonadRandom m) => TestData -> TAddress (MarketplaceTezPermitEntrypoints ()) -> m ()
buyAllOffchain TestData{testTokenAmount} contract =
  replicateM_ (fromIntegral testTokenAmount) $
    offchainBuy contract