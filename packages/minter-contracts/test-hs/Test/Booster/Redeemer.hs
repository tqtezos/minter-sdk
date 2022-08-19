module Test.Booster.Redeemer where

import Data.Int ()
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Foldable as F
import qualified Data.Set as Set 
import Data.Maybe
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
      let cards = List.map (\n -> Booster.GlobalTokenId cardFA2Address n) cardIds
      let addPacksParam = List.map 
                         (\n -> 
                           (,) (Booster.GlobalTokenId packFA2Address n) -- Pack token Id matches PackId
                               (fst $ fromJust $ Map.lookup (Booster.PackId n) testValidHashes) --Hash for that pack 
                         ) 
                         packIds
      withSender seller $
        addTokens cards boosterContract 
      withSender seller $
        addPacks addPacksParam boosterContract 
      
      forM_ packIds \packId -> do 
        let redeemParam =  (snd $ fromJust $ Map.lookup (Booster.PackId packId) testValidHashes) -- key
        let packContentsDeltas = List.concatMap (\tokenId -> [((buyer, TokenId tokenId), 1), ((seller, TokenId tokenId), -1)]) 
                                          (Booster.tokensContained redeemParam) 
        assertingBalanceDeltas packFA2
          [  (buyer, TokenId packId) -: -1
           , (seller, TokenId packId) -: 1
          ] $ do
            assertingBalanceDeltas cardFA2 packContentsDeltas
               $ do
                withSender buyer $ 
                  redeem redeemParam boosterContract

hprop_Redeeming_with_incorrect_nonce_fails :: Property
hprop_Redeeming_with_incorrect_nonce_fails =
  property $ do
    testData@TestData{testValidHashes} <- forAll genTestData

    clevelandProp $ do
      setup@Setup{seller, buyer, cardFA2, packFA2, boosterContract} <- testSetup testData
      let packFA2Address = toAddress packFA2
      let cardFA2Address = toAddress cardFA2
      let cards = List.map (\n -> Booster.GlobalTokenId cardFA2Address n) cardIds
      let addPacksParam = List.map 
                         (\n -> 
                           (,) (Booster.GlobalTokenId packFA2Address n) -- Pack token Id matches PackId
                               (fst $ fromJust $ Map.lookup (Booster.PackId n) testValidHashes) --Hash for that pack 
                         ) 
                         packIds
      withSender seller $
        addTokens cards boosterContract 
      withSender seller $
        addPacks addPacksParam boosterContract 
      
      forM_ packIds \packId -> do
        let redeemData = fromJust $ Map.lookup (Booster.PackId packId) testValidHashes
        let redeemHash =  fst $ redeemData 
        let redeemParam =  snd $ redeemData -- key
        let fabricatedRedeemParam = redeemParam {Booster.nonce = 0}
        withSender buyer $
          when (fabricatedRedeemParam /= redeemParam) 
              (redeem fabricatedRedeemParam boosterContract
                & expectFailedWith (([mt|HASHES_DONT_MATCH|], (mkHash fabricatedRedeemParam)), redeemHash))
    
hprop_Redeeming_with_incorrect_pack_fails :: Property
hprop_Redeeming_with_incorrect_pack_fails =
  property $ do
    testData@TestData{testValidHashes} <- forAll genTestData

    clevelandProp $ do
      setup@Setup{seller, buyer, cardFA2, packFA2, boosterContract} <- testSetup testData
      let packFA2Address = toAddress packFA2
      let cardFA2Address = toAddress cardFA2
      let cards = List.map (\n -> Booster.GlobalTokenId cardFA2Address n) cardIds
      let addPacksParam = List.map 
                         (\n -> 
                           (,) (Booster.GlobalTokenId packFA2Address n) -- Pack token Id matches PackId
                               (fst $ fromJust $ Map.lookup (Booster.PackId n) testValidHashes) --Hash for that pack 
                         ) 
                         packIds
      withSender seller $
        addTokens cards boosterContract 
      withSender seller $
        addPacks addPacksParam boosterContract 
      
      forM_ packIds \packId -> do
        let redeemData = fromJust $ Map.lookup (Booster.PackId packId) testValidHashes
        let redeemHash =  fst $ redeemData 
        let redeemParam =  snd $ redeemData -- key
        let fabricatedRedeemParam = redeemParam {Booster.tokensContained = [1]}
        withSender buyer $
          when (fabricatedRedeemParam /= redeemParam) 
              (redeem fabricatedRedeemParam boosterContract
                & expectFailedWith (([mt|HASHES_DONT_MATCH|], (mkHash fabricatedRedeemParam)), redeemHash))
    

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

newtype TestData = TestData
  { testValidHashes :: Map Booster.PackId  (ByteString, Booster.RedeemKey) 
  } 
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
  shuffledTokenIds :: [Natural] <- Gen.shuffle cardIds
  splitLengths <- replicateM 10 (Gen.int (Range.linear 1 10))
  let packs = Split.splitPlaces splitLengths shuffledTokenIds -- generates 10 random packs of lengths 1 to 10 each 
  nonces <- replicateM 10 (Gen.integral (Range.linear 1 1000000)) -- generates 10 nonces 
  let redeemKeys =  (packIds `zip` packs `zip` nonces) <&> (\((packId, tokens), nonce) -> 
          Booster.RedeemKey 
          {
            packId = Booster.PackId packId, 
            tokensContained = tokens,
            nonce = nonce 
          }
        )
  let keyValues =  List.map (\key -> (Booster.packId key, (mkHash key, key))) redeemKeys
  let testValidHashes = Map.fromList keyValues
  pure $ TestData { testValidHashes = testValidHashes }

testSetup :: MonadNettest caps base m => TestData -> m Setup
testSetup testData = do
  seller <- newAddress "seller"
  buyer <- newAddress "buyer"

  let boosterStorage =
        Booster.initBoosterStorage $
         SimpleAdmin.initAdminStorage seller

  let ledger =
        Map.fromListWith (+) $
          cardTokens <&> \tokenId -> ((seller, tokenId), 1)
  let packLedger =
        Map.fromListWith (+) $
          packTokens <&> \tokenId -> ((buyer, tokenId), 1)
  
  boosterContract <- originateBoosterContract boosterStorage 

   -- Create an FA2 contracts, and give the card assets to the seller.
  cardFA2 <- originateSimple "card_fa2"
     FA2.Storage
     { sLedger = BigMap ledger
     , sOperators = BigMap $ Map.fromList [((seller, toAddress boosterContract), ())]
     , sTokenMetadata = mempty
     }
     (FA2.fa2Contract def { FA2.cAllowedTokenIds = cardTokens })

   -- Create an FA2 contracts, and give the pack assets to the buyer
  packFA2 <- originateSimple "pack_fa2"
     FA2.Storage
     { sLedger = BigMap packLedger
     , sOperators = BigMap $ Map.fromList [((buyer, toAddress boosterContract), ())]
     , sTokenMetadata = mempty
     }
     (FA2.fa2Contract def { FA2.cAllowedTokenIds = packTokens })

  pure Setup {..}

cardIds :: [Natural]
cardIds = [0..99]

packIds :: [Natural]
packIds = [0..9]

cardTokens :: [TokenId]
cardTokens = TokenId `List.map` cardIds

packTokens :: [TokenId]
packTokens = TokenId `List.map` packIds

originateBoosterContract :: MonadNettest caps base m => Booster.BoosterStorage ->  m $ ContractHandler Booster.BoosterEntrypoints Booster.BoosterStorage
originateBoosterContract storage = do
  originateSimple "booster-redeemer" storage BoosterContract.boosterContract

mkHash :: Booster.RedeemKey -> ByteString
mkHash redeemKey = 
  blake2b $ packValue' $ toVal (packId, (tokensContained, nonce))
  where tokensContained = Booster.tokensContained redeemKey 
        nonce = Booster.nonce redeemKey
        packId = Booster.packId redeemKey
----------------------------------------------------------------------------
-- Call entrypoints
----------------------------------------------------------------------------

addPacks :: (HasCallStack, MonadNettest caps base m) => [(Booster.GlobalTokenId, ByteString)] -> ContractHandler Booster.BoosterEntrypoints Booster.BoosterStorage -> m ()
addPacks packs contract=
  call contract (Call @"Add_packs") packs


addTokens :: (HasCallStack, MonadNettest caps base m) => [Booster.GlobalTokenId] -> ContractHandler Booster.BoosterEntrypoints Booster.BoosterStorage -> m ()
addTokens tokens contract =
  call contract (Call @"Add_tokens") tokens

redeem :: (HasCallStack, MonadNettest caps base m) => Booster.RedeemKey -> ContractHandler Booster.BoosterEntrypoints Booster.BoosterStorage -> m ()
redeem redeemParam contract =
  call contract (Call @"Redeem_booster") redeemParam