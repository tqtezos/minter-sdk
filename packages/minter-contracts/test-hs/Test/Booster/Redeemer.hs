module Test.Booster.Redeemer where

import Data.Int ()
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Foldable as F
import qualified Data.Set as Set 
import Data.List.Split as Split

import Hedgehog (Gen, Property, forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Morley.Nettest

import Hedgehog.Gen.Tezos.Core (genMutez')
import qualified Indigo.Contracts.FA2Sample as FA2
import Lorentz hiding (balance, contract, sha256, get)
import Michelson.Interpret.Pack

import qualified Lorentz.Contracts.Booster.Redeemer as Booster
import qualified Lorentz.Contracts.Booster.Contracts as BoosterContract
import Lorentz.Contracts.Spec.FA2Interface (TokenId(TokenId))
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Test.Util
import Tezos.Crypto

import qualified Lorentz.Contracts.SimpleAdmin as SimpleAdmin

hprop_Redeeming_with_correct_key_succeeds :: Property
hprop_Redeeming_with_correct_key_succeeds =
  property $ do
    testData@TestData{testValidHashes} <- forAll genTestData

    clevelandProp $ do
      setup@Setup{seller, buyer, cardFA2, packFA2, boosterContract} <- testSetup testData
      let packFA2Address = toAddress packFA2
      let cardFA2Address = toAddress cardFA2
      --let packs = List.map (\(TokenId n) -> GlobalTokenId packFA2Address n) packIds
      let cards = List.map (\(TokenId n) -> GlobalTokenId cardFA2Address n) tokenIds
      --withSender seller $
      --  addPacks boosterContract packs
      withSender seller $
        addTokens boosterContract cards



----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

newtype TestData = TestData
  { testValidHashes :: Map ByteString Booster.RedeemKey } 
  deriving stock (Show)

data Setup = Setup
  { seller :: Address
  , buyer :: Address
  , cardFA2 :: ContractHandler FA2.FA2SampleParameter FA2.Storage
  , packFA2 :: ContractHandler FA2.FA2SampleParameter FA2.Storage
  , boosterContract :: ContractHandler Booster.BoosterEntrypoints Booster.BoosterStorage
  }

genTestData :: Gen TestData
genTestData = do
  shuffledTokenIds :: [Natural] <- Gen.shuffle [0 .. 99]
  splitLengths <- replicateM 10 (Gen.int (Range.linear 1 10))
  let packs = Split.splitPlaces splitLengths shuffledTokenIds -- generates 10 random packs of lengths 1 to 10 each 
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

  let boosterStorage =
        Booster.initBoosterStorage $
         SimpleAdmin.initAdminStorage seller

  let ledger =
        Map.fromListWith (+) $
          tokenIds <&> \tokenId -> ((seller, tokenId), 1)
  let packLedger =
        Map.fromListWith (+) $
          packIds <&> \packId -> ((buyer, packId), 1)
  
  boosterContract <- originateBoosterContract boosterStorage 

   -- Create an FA2 contracts, and give the card assets to the seller.
  cardFA2 <- originateSimple "card_fa2"
     FA2.Storage
     { sLedger = BigMap ledger
     , sOperators = BigMap $ Map.fromList [((seller, toAddress boosterContract), ())]
     , sTokenMetadata = mempty
     }
     (FA2.fa2Contract def { FA2.cAllowedTokenIds = tokenIds })

   -- Create an FA2 contracts, and give the pack assets to the buyer
  packFA2 <- originateSimple "pack_fa2"
     FA2.Storage
     { sLedger = BigMap packLedger
     , sOperators = BigMap $ Map.fromList [((buyer, toAddress boosterContract), ())]
     , sTokenMetadata = mempty
     }
     (FA2.fa2Contract def { FA2.cAllowedTokenIds = packIds })

  pure Setup {..}

tokenIds :: [TokenId]
tokenIds = TokenId `List.map` [0..99]

packIds :: [TokenId]
packIds = TokenId `List.map` [0..9]

originateBoosterContract :: MonadNettest caps base m => Booster.BoosterStorage ->  m $ ContractHandler Booster.BoosterEntrypoints Booster.BoosterStorage
originateBoosterContract storage = do
  originateSimple "booster-redeemer" storage BoosterContract.boosterContract

mkHash :: Booster.RedeemKey -> ByteString
mkHash redeemKey = 
  sha256 $ packValue' (toVal redeemKey)
  
----------------------------------------------------------------------------
-- Call entrypoints
----------------------------------------------------------------------------

addPacks :: (HasCallStack, MonadNettest caps base m) => [(Booster.GlobalTokenId, ByteString)] -> ContractHandler Booster.BoosterEntrypoints Booster.BoosterStorage -> m ()
addPacks contract packs =
  call contract (Call @"Add_packs") packs


addTokens :: (HasCallStack, MonadNettest caps base m) => [Booster.GlobalTokenId] -> ContractHandler Booster.BoosterEntrypoints Booster.BoosterStorage -> m ()
addTokens contract tokens =
  call contract (Call @"Add_tokens") tokens


