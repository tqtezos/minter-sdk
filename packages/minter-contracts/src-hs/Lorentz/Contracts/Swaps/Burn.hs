-- | Lorentz bindings for the allowlisted swaps contract.
module Lorentz.Contracts.Swaps.Burn where

import Lorentz

import Lorentz.Contracts.MinterSdk
import Michelson.Test.Import (embedContractM)
import qualified Michelson.Typed as T

import Tezos.Address (unsafeParseAddress)
import Lorentz.Contracts.NonPausableSimpleAdmin

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

initBurnSwapStorage :: BurnSwapStorage
initBurnSwapStorage = BurnSwapStorage
  { nextSwapId = SwapId 0
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

-- Contract
----------------------------------------------------------------------------

allowlistedBurnSwapsContract
  :: T.Contract (ToT AllowlistedBurnSwapEntrypoints) (ToT AllowlistedBurnSwapStorage)
allowlistedBurnSwapsContract =
  $$(embedContractM (inBinFolder "fa2_allowlisted_swap_with_burn.tz"))
