-- | Lorentz bindings for the swaps contract.
module Lorentz.Contracts.Booster.Redeemer where

import Lorentz

import Lorentz.Contracts.Spec.FA2Interface
import qualified Lorentz.Contracts.SimpleAdmin as SimpleAdmin

-- Types
----------------------------------------------------------------------------

newtype PackId = PackId Natural
  deriving stock (Show, Eq, Ord)
  deriving newtype (IsoValue, HasAnnotation)

--newtype TokenRegistryId = TokenRegistryId Natural
--  deriving stock (Show, Eq, Ord)
--  deriving newtype (IsoValue, HasAnnotation)

data RedeemKey = RedeemKey
  { tokensContained :: [Natural]
  , nonce :: Natural
  } deriving stock (Eq, Show)

customGeneric "RedeemKey" ligoCombLayout
deriving anyclass instance IsoValue RedeemKey
deriving anyclass instance HasAnnotation RedeemKey

data RedeemParam = RedeemParam
  { packOwner :: Address
  , packId :: PackId
  , redeemKey :: RedeemKey
  }

customGeneric "RedeemParam" ligoCombLayout
deriving anyclass instance IsoValue RedeemParam
deriving anyclass instance HasAnnotation RedeemParam

data GlobalTokenId = GlobalTokenId
  { fa2Address :: Address
  , tokenId :: Natural
  }
  deriving stock (Eq, Show)

customGeneric "GlobalTokenId" ligoCombLayout
deriving anyclass instance IsoValue GlobalTokenId
deriving anyclass instance HasAnnotation GlobalTokenId

data BoosterEntrypoints = Add_packs [(GlobalTokenId, ByteString)]
  | Add_tokens [GlobalTokenId]
  | Redeem_booster RedeemParam
  | Admin SimpleAdmin.AdminEntrypoints

customGeneric "BoosterEntrypoints" ligoLayout
deriving anyclass instance IsoValue BoosterEntrypoints
deriving anyclass instance HasAnnotation BoosterEntrypoints

instance
  ( RequireAllUniqueEntrypoints BoosterEntrypoints
  , EntrypointsDerivation EpdDelegate BoosterEntrypoints
  ) =>
    ParameterHasEntrypoints BoosterEntrypoints where
  type ParameterEntrypointsDerivation BoosterEntrypoints = EpdDelegate


data BoosterStorage = BoosterStorage
  { nextPackId :: PackId
  , nextTokenRegistryId :: Natural
  , packs :: BigMap PackId (GlobalTokenId, ByteString)
  , tokenRegistry :: BigMap Natural GlobalTokenId
  , admin :: SimpleAdmin.AdminStorage
  }

customGeneric "BoosterStorage" ligoCombLayout
deriving anyclass instance IsoValue BoosterStorage
deriving anyclass instance HasAnnotation BoosterStorage

initBoosterStorage :: SimpleAdmin.AdminStorage -> BoosterStorage 
initBoosterStorage as = BoosterStorage
  { nextPackId = PackId 0
  , nextTokenRegistryId = 0
  , packs = mempty
  , tokenRegistry = mempty
  , admin = as
  }


incrementPackId :: PackId -> PackId
incrementPackId (PackId n) = PackId (n + 1)

initPackId :: PackId
initPackId = PackId 1

getPackId :: PackId -> Natural
getPackId (PackId n) = n

-- Errors
----------------------------------------------------------------------------
