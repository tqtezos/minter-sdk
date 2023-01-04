
-- | Lorentz bindings for NFT multi-asset contract
module Lorentz.Contracts.MinterCollection.Nft.Types where

import Fmt (Buildable(..), genericF)
import Lorentz

import Lorentz.Contracts.SimpleAdmin (AdminEntrypoints(..), AdminStorage(..))
import qualified Lorentz.Contracts.FA2 as FA2

import Lorentz.Contracts.Spec.FA2Interface (TokenId(..))

-- TODO: move to Lorentz.Contracts.SimpleAdmin
import Lorentz.Contracts.BondingCurve.Interface (exampleAdminStorage)

-- type nft_meta = (token_id, token_metadata) big_map
-- type ledger = (token_id, address) big_map
-- type nft_token_storage = {
--   ledger : ledger;
--   token_metadata : nft_meta;
--   next_token_id : token_id;
--   operators : operator_storage;
-- }
data NftTokenStorage = NftTokenStorage
  { ledger :: BigMap TokenId Address
  , token_metadata :: BigMap TokenId FA2.TokenMetadata
  , next_token_id :: TokenId
  , operators :: FA2.OperatorStorage
  } deriving stock (Eq, Show)

customGeneric "NftTokenStorage" ligoLayout
deriving anyclass instance IsoValue NftTokenStorage
deriving anyclass instance HasAnnotation NftTokenStorage
instance Buildable NftTokenStorage where build = genericF

exampleNftTokenStorage :: NftTokenStorage
exampleNftTokenStorage = NftTokenStorage
  { ledger = mempty
  , token_metadata = mempty
  , next_token_id = TokenId 0
  , operators = mempty
  }

-- type nft_asset_storage = {
--   assets : nft_token_storage;
--   admin : admin_storage;
--   metadata: (string, bytes) big_map; (* contract metadata *)
-- }
data NftStorage = NftStorage
  { assets :: NftTokenStorage
  , admin :: AdminStorage
  , metadata :: BigMap MText ByteString
  } deriving stock (Eq, Show)

customGeneric "NftStorage" ligoLayout
deriving anyclass instance IsoValue NftStorage
deriving anyclass instance HasAnnotation NftStorage
instance Buildable NftStorage where build = genericF

exampleNftStorage :: NftStorage
exampleNftStorage = NftStorage
  { assets = exampleNftTokenStorage
  , admin = exampleAdminStorage
  , metadata = mempty
  }

-- | exampleNftStorage with admin set
exampleNftStorageWithAdmin :: Address -> NftStorage
exampleNftStorageWithAdmin admin =
  exampleNftStorage { admin = AdminStorage admin Nothing False }

---- | Print properly-formatted michelson values for exampleStorage
----
---- ("admin","Pair (Pair \"tz2C97sask3WgSSg27bJhFxuBwW6MMU4uTPK\" False) None")
---- ("market_contract","\"tz2UXHa5WU79MnWF5uKFRM6qUowX13pdgJGy\"")
---- "{ Pair (Pair \"tz2C97sask3WgSSg27bJhFxuBwW6MMU4uTPK\" False) None; \"tz2UXHa5WU79MnWF5uKFRM6qUowX13pdgJGy\"; 0; 1; 2; 100; Pair { Pair 6 { 7; 8 } } { 4; 5 }; 3 }"
--printExampleStorage' :: IO ()
--printExampleStorage' = do
--  print $ ("admin" :: String, printLorentzValue False exampleAdminStorage)
--  print $ ("market_contract" :: String, printLorentzValue False $ market_contract exampleStorage')
--  print $ printLorentzValue False exampleStorage'


-- type mint_tokens_param = mint_token_param list
type MintTokensParam = [MintTokenParam]

-- type mint_token_param =
-- [@layout:comb]
-- {
--   token_metadata: token_metadata;
--   owner : address;
-- }
data MintTokenParam = MintTokenParam
  { token_metadata :: FA2.TokenMetadata
  , owner :: Address
  } deriving stock (Eq, Show)

customGeneric "MintTokenParam" ligoCombLayout
deriving anyclass instance IsoValue MintTokenParam
deriving anyclass instance HasAnnotation MintTokenParam


-- type nft_asset_entrypoints =
--   | Assets of fa2_entry_points
--   | Mint of mint_tokens_param
--   | Burn of (token_id * bytes)
--   | Update_metadata of (token_metadata list)
--   | Admin of admin_entrypoints
data NftEntrypoints
  = Assets FA2.Parameter
  | Mint MintTokensParam
  | Burn (TokenId, (ByteString, Address))
  | Update_metadata [FA2.TokenMetadata]
  | Admin AdminEntrypoints
  deriving stock (Eq, Show)

customGeneric "NftEntrypoints" ligoLayout
deriving anyclass instance IsoValue NftEntrypoints
deriving anyclass instance HasAnnotation NftEntrypoints

instance ParameterHasEntrypoints NftEntrypoints where
  -- EpdRecursive so that AdminEntrypoints are reached
  type ParameterEntrypointsDerivation NftEntrypoints = EpdRecursive

