module Test.Booster.Redeemer where

import Data.Int ()
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Foldable as F
import qualified Data.Set as Set 

import Hedgehog (Gen, Property, forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Morley.Nettest

import Hedgehog.Gen.Tezos.Core (genMutez')
import qualified Indigo.Contracts.FA2Sample as FA2
import Lorentz hiding (balance, contract, sha256)
import Michelson.Interpret.Pack

import qualified Lorentz.Contracts.Booster.Redeemer as Booster
import Lorentz.Contracts.Spec.FA2Interface (TokenId(TokenId))
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Test.Util
import Tezos.Crypto

import qualified Lorentz.Contracts.SimpleAdmin as SimpleAdmin

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

newtype TestData = TestData
  { testValidHashes :: Map ByteString Booster.RedeemKey } 
  deriving stock (Show)

data Setup = Setup
  { seller :: Address
  , buyer :: Address
  , assetFA2 :: ContractHandler FA2.FA2SampleParameter FA2.Storage
  , storage :: Booster.BoosterStorage
  }

genTestData :: Gen TestData
genTestData = do
  packs <- replicateM 10 (Gen.list (Range.linear 1 5) (Gen.integral (Range.linear 0 9))) --generates a list of 1 to 5 tokens having ids 0 to 9, 10 times
  nonces <- replicateM 10 (Gen.integral (Range.linear 1 1000000)) -- generates 10 nonces   
  let redeemKeys =  ([1 .. 10] `zip` packs `zip` nonces) <&> (\((packId, tokens), nonce) -> 
          Booster.RedeemKey 
          {
            packId = Booster.PackId packId, 
            tokensContained = Booster.TokenRegistryId `List.map` tokens,
            nonce = nonce 
          })
  let keyValues =  List.map (\key -> (mkHash key, key)) redeemKeys
  let testValidHashes = Map.fromList keyValues
  pure $ TestData { testValidHashes = testValidHashes}

testSetup :: MonadNettest caps base m => TestData -> m Setup
testSetup testData = do
  seller <- newAddress "seller"
  buyer <- newAddress "buyer"

  let storage =
        Booster.initBoosterStorage $
         SimpleAdmin.initAdminStorage seller


  -- Create an FA2 contracts, and give the asset to the seller.
  assetFA2 <- originateSimple "asset_fa2"
    FA2.Storage
    { sLedger = BigMap $ Map.fromList [((seller, TokenId 1), 1)]
    , sOperators = mempty
    , sTokenMetadata = mempty
    }
    (FA2.fa2Contract def { FA2.cAllowedTokenIds = [TokenId 1] })

  pure Setup {..}

originateBoosterContract :: MonadNettest caps base m => Setup -> Contract Booster.BoosterEntrypoints Booster.BoosterStorage -> m $ ContractHandler Booster.BoosterEntrypoints Booster.BoosterStorage
originateBoosterContract Setup{storage, seller, assetFA2} boosterContract = do
  contract <- originateSimple "booster-redeemer" storage boosterContract

  -- Make the contract an operator for the seller.
  withSender seller $ do
    call assetFA2 (Call @"Update_operators")
      [ FA2.AddOperator FA2.OperatorParam
          { opOwner = seller
          , opOperator = toAddress contract
          , opTokenId = TokenId 1
          }
      ]

  pure contract

----------------------------------------------------------------------------
-- Call entrypoints
----------------------------------------------------------------------------

mkHash :: Booster.RedeemKey -> ByteString
mkHash redeemKey = 
  sha256 $ packValue' (toVal redeemKey)
  
