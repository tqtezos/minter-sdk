-- | Lorentz bindings for the swaps contract.
module Lorentz.Contracts.Booster.Redeemer where

import Lorentz

import Lorentz.Contracts.Spec.FA2Interface
import Lorentz.Contracts.SimpleAdmin

-- Types
----------------------------------------------------------------------------

newtype PackId = PackId Natural
  deriving stock (Show, Eq, Ord)
  deriving newtype (IsoValue, HasAnnotation)

newtype TokenRegistryId = TokenRegistryId Natural
  deriving stock (Show, Eq, Ord)
  deriving newtype (IsoValue, HasAnnotation)

data RedeemKey = RedeemKey
  { packId :: PackId
  , tokensContained :: [TokenRegistryId]
  , nonce :: Natural
  }

customGeneric "RedeemKey" ligoCombLayout
deriving anyclass instance IsoValue RedeemKey
deriving anyclass instance HasAnnotation RedeemKey

data RedeemParam = RedeemParam
  { packOwner :: Address
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

data BoosterEntrypoints = 
    Add_packs [(GlobalTokenId, ByteString)]
  | Add_tokens [GlobalTokenId]
  | Redeem_booster RedeemParam
  | Admin AdminEntrypoints

customGeneric "BoosterEntrypoints" ligoLayout
deriving anyclass instance IsoValue BoosterEntrypoints
deriving anyclass instance HasAnnotation BoosterEntrypoints

instance ParameterHasEntrypoints BoosterEntrypoints where
  type ParameterEntrypointsDerivation BoosterEntrypoints = EpdPlain

data BoosterStorage = BoosterStorage
  { nextPackId :: PackId
  , nextTokenRegistryId :: TokenRegistryId
  , packs :: BigMap PackId (GlobalTokenId, ByteString)
  , tokenRegistry :: BigMap Natural GlobalTokenId
  , tokensOwner :: Address 
  , admin :: AdminStorage
  }

customGeneric "BoosterStorage" ligoCombLayout
deriving anyclass instance IsoValue BoosterStorage
deriving anyclass instance HasAnnotation BoosterStorage

incrementPackId :: PackId -> PackId
incrementPackId (PackId n) = PackId (n + 1)

initPackId :: PackId
initPackId = PackId 1

getPackId :: PackId -> Natural
getPackId (PackId n) = n

-- Errors
----------------------------------------------------------------------------

errSwapNotExist :: MText
errSwapNotExist = [mt|SWAP_NOT_EXIST|]

errSwapFinished :: MText
errSwapFinished = [mt|SWAP_NOT_EXIST|]

errSwapCancelled :: MText
errSwapCancelled = [mt|SWAP_NOT_EXIST|]
-- ↑ The contract does not actually distinguish these cases

errNotSwapSeller :: MText
errNotSwapSeller = [mt|NOT_SWAP_SELLER|]

errSwapOfferedFA2Invalid :: MText
errSwapOfferedFA2Invalid = [mt|SWAP_OFFERED_FA2_INVALID|]

errSwapRequestedFA2Invalid :: MText
errSwapRequestedFA2Invalid = [mt|SWAP_REQUESTED_FA2_INVALID|]

errSwapRequestedFA2BalanceInvalid :: Natural -> Natural -> (MText, Natural, Natural)
errSwapRequestedFA2BalanceInvalid requested actual = ([mt|FA2_INSUFFICIENT_BALANCE|], requested, actual)
