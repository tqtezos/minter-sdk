-- | Lorentz bindings for the swap with permit contract
module Lorentz.Contracts.Swaps.SwapPermitBurnFee where

import Lorentz

import Lorentz.Contracts.MinterSdk
import Michelson.Test.Import (embedContractM)
import qualified Michelson.Typed as T

import Lorentz.Contracts.NonPausableSimpleAdmin
import Lorentz.Contracts.Swaps.Basic
import Lorentz.Contracts.Swaps.Burn
import Lorentz.Contracts.Swaps.SwapPermit
import Lorentz.Contracts.Swaps.AllowlistedFee as AllowlistedFee 
import Lorentz.Contracts.Swaps.Allowlisted
-- Types
----------------------------------------------------------------------------

data BurnSwapFeeStorage = BurnSwapFeeStorage
  { nextSwapId :: SwapId
  , swaps :: BigMap SwapId AllowlistedFee.SwapInfo
  , burnAddress :: Address
  }

customGeneric "BurnSwapFeeStorage" ligoLayout
deriving anyclass instance IsoValue BurnSwapFeeStorage
deriving anyclass instance HasAnnotation BurnSwapFeeStorage

initBurnSwapFeeStorage :: BurnSwapFeeStorage
initBurnSwapFeeStorage = BurnSwapFeeStorage
  { nextSwapId = initSwapId
  , swaps = mempty
  , burnAddress = nullAddress
  }

data AllowlistedBurnSwapFeeStorage = AllowlistedBurnSwapFeeStorage
  { burnSwapStorage :: BurnSwapFeeStorage
  , admin :: AdminStorage
  , allowlist :: AllowlistedFee.Allowlist
  }

customGeneric "AllowlistedBurnSwapFeeStorage" ligoLayout
deriving anyclass instance IsoValue AllowlistedBurnSwapFeeStorage
deriving anyclass instance HasAnnotation AllowlistedBurnSwapFeeStorage

initAllowlistedBurnSwapFeeStorage :: Address -> AllowlistedBurnSwapFeeStorage
initAllowlistedBurnSwapFeeStorage admin = AllowlistedBurnSwapFeeStorage
  { burnSwapStorage = initBurnSwapFeeStorage
  , admin = initAdminStorage admin
  , allowlist = mempty
  }

data PermitSwapBurnFeeEntrypoints
  = BaseSwap AllowlistedFeeSwapEntrypoints
  | Offchain_accept [OffchainAcceptParam]

customGeneric "PermitSwapBurnFeeEntrypoints" ligoLayout
deriving anyclass instance IsoValue PermitSwapBurnFeeEntrypoints
deriving anyclass instance HasAnnotation PermitSwapBurnFeeEntrypoints

instance ParameterHasEntrypoints PermitSwapBurnFeeEntrypoints where
  type ParameterEntrypointsDerivation PermitSwapBurnFeeEntrypoints = EpdDelegate

-- Contract
----------------------------------------------------------------------------

allowlistedSwapsPermitBurnFeeContract
  :: T.Contract (ToT PermitSwapBurnFeeEntrypoints) (ToT AllowlistedBurnSwapFeeStorage)
allowlistedSwapsPermitBurnFeeContract =
  $$(embedContractM (inBinFolder "fa2_swap_offchain_burn_fee.tz"))

-- Errors
----------------------------------------------------------------------------