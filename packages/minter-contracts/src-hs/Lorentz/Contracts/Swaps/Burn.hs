-- | Lorentz bindings for the allowlisted swaps contract.
module Lorentz.Contracts.Swaps.Burn where

import Lorentz

import Lorentz.Contracts.NonPausableSimpleAdmin
import Tezos.Address (unsafeParseAddress)

import Lorentz.Contracts.Swaps.Allowlisted
import Lorentz.Contracts.Swaps.Basic

-- Types
----------------------------------------------------------------------------

data BurnSwapStorage = BurnSwapStorage
  { nextSwapId :: SwapId
  , swaps :: BigMap SwapId SwapInfo
  , burnAddress :: Address
  }

customGeneric "BurnSwapStorage" ligoLayout
deriving anyclass instance IsoValue BurnSwapStorage
deriving anyclass instance HasAnnotation BurnSwapStorage

nullAddress :: Address
nullAddress = unsafeParseAddress "tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU"

altBurnAddress :: Address
altBurnAddress = unsafeParseAddress "tz1burnburnburnburnburnburnburjAYjjX"

initBurnSwapStorage :: BurnSwapStorage
initBurnSwapStorage = BurnSwapStorage
  { nextSwapId = initSwapId
  , swaps = mempty
  , burnAddress = nullAddress
  }

data AllowlistedBurnSwapStorage = AllowlistedBurnSwapStorage
  { burnSwapStorage :: BurnSwapStorage
  , admin :: AdminStorage
  , allowlist :: Allowlist
  }

customGeneric "AllowlistedBurnSwapStorage" ligoLayout
deriving anyclass instance IsoValue AllowlistedBurnSwapStorage
deriving anyclass instance HasAnnotation AllowlistedBurnSwapStorage

initAllowlistedBurnSwapStorage :: Address -> AllowlistedBurnSwapStorage
initAllowlistedBurnSwapStorage admin = AllowlistedBurnSwapStorage
  { burnSwapStorage = initBurnSwapStorage
  , admin = initAdminStorage admin
  , allowlist = mempty
  }

data AllowlistedBurnSwapEntrypoints
  = Swap SwapEntrypoints
  | Admin AdminEntrypoints
  | Update_allowed (BigMap Address ())

customGeneric "AllowlistedBurnSwapEntrypoints" ligoLayout
deriving anyclass instance IsoValue AllowlistedBurnSwapEntrypoints
deriving anyclass instance HasAnnotation AllowlistedBurnSwapEntrypoints

instance ParameterHasEntrypoints AllowlistedBurnSwapEntrypoints where
  type ParameterEntrypointsDerivation AllowlistedBurnSwapEntrypoints = EpdDelegate


-- Used for testing
data ChangeBurnAddressSwapEntrypoints
  = Swap' SwapEntrypoints
  | Admin' AdminEntrypoints
  | Update_allowed' (BigMap Address ())
  | Change_burn_address Address

customGeneric "ChangeBurnAddressSwapEntrypoints" ligoLayout
deriving anyclass instance IsoValue ChangeBurnAddressSwapEntrypoints
deriving anyclass instance HasAnnotation ChangeBurnAddressSwapEntrypoints

instance ParameterHasEntrypoints ChangeBurnAddressSwapEntrypoints where
  type ParameterEntrypointsDerivation ChangeBurnAddressSwapEntrypoints = EpdDelegate
