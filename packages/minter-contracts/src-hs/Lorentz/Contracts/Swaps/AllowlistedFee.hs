-- | Lorentz bindings for the allowlisted swaps contract.
module Lorentz.Contracts.Swaps.AllowlistedFee where

import Lorentz

import Lorentz.Contracts.MinterSdk
import Michelson.Test.Import (embedContractM)
import qualified Michelson.Typed as T

import Lorentz.Contracts.NonPausableSimpleAdmin
import Lorentz.Contracts.Swaps.Basic hiding (SwapOffer, SwapInfo, SwapEntrypoints, SwapStorage, initSwapStorage) 

-- Types
----------------------------------------------------------------------------
data SwapOffer = SwapOffer
  { assetsOffered :: [FA2Assets]
  , assetsRequested :: ([FA2Assets], Mutez)
  }

customGeneric "SwapOffer" ligoCombLayout
deriving anyclass instance IsoValue SwapOffer
deriving anyclass instance HasAnnotation SwapOffer

data SwapInfo = SwapInfo
  { swapOffer :: SwapOffer
  , seller :: Address
  }

customGeneric "SwapInfo" ligoCombLayout
deriving anyclass instance IsoValue SwapInfo
deriving anyclass instance HasAnnotation SwapInfo

data SwapEntrypoints
  = Start SwapOffer
  | Cancel SwapId
  | Accept SwapId

customGeneric "SwapEntrypoints" ligoLayout
deriving anyclass instance IsoValue SwapEntrypoints
deriving anyclass instance HasAnnotation SwapEntrypoints

instance ParameterHasEntrypoints SwapEntrypoints where
  type ParameterEntrypointsDerivation SwapEntrypoints = EpdPlain

data SwapStorage = SwapStorage
  { nextSwapId :: SwapId
  , swaps :: BigMap SwapId SwapInfo
  }

customGeneric "SwapStorage" ligoLayout
deriving anyclass instance IsoValue SwapStorage
deriving anyclass instance HasAnnotation SwapStorage

initSwapStorage :: SwapStorage
initSwapStorage = SwapStorage
  { nextSwapId = SwapId 0
  , swaps = mempty
  }

type Allowlist = BigMap Address ()

data AllowlistedFeeSwapStorage = AllowlistedFeeSwapStorage
  { swapStorage :: SwapStorage
  , admin :: AdminStorage
  , allowlist :: Allowlist
  }

customGeneric "AllowlistedFeeSwapStorage" ligoLayout
deriving anyclass instance IsoValue AllowlistedFeeSwapStorage
deriving anyclass instance HasAnnotation AllowlistedFeeSwapStorage

initAllowlistedFeeSwapStorage :: Address -> AllowlistedFeeSwapStorage
initAllowlistedFeeSwapStorage admin = AllowlistedFeeSwapStorage
  { swapStorage = initSwapStorage
  , admin = initAdminStorage admin
  , allowlist = mempty
  }

data AllowlistedFeeSwapEntrypoints
  = Swap SwapEntrypoints
  | Admin AdminEntrypoints
  | Update_allowed (BigMap Address ())

customGeneric "AllowlistedFeeSwapEntrypoints" ligoLayout
deriving anyclass instance IsoValue AllowlistedFeeSwapEntrypoints
deriving anyclass instance HasAnnotation AllowlistedFeeSwapEntrypoints

instance ParameterHasEntrypoints AllowlistedFeeSwapEntrypoints where
  type ParameterEntrypointsDerivation AllowlistedFeeSwapEntrypoints = EpdDelegate

-- Contract
----------------------------------------------------------------------------

allowlistedFeeSwapsContract
  :: T.Contract (ToT AllowlistedFeeSwapEntrypoints) (ToT AllowlistedFeeSwapStorage)
allowlistedFeeSwapsContract =
  $$(embedContractM (inBinFolder "fa2_fee_allowlisted_swap.tz"))

-- Errors
----------------------------------------------------------------------------

errSwapOfferedNotAllowlisted :: MText
errSwapOfferedNotAllowlisted = [mt|SWAP_OFFERED_FA2_NOT_ALLOWLISTED|]

errSwapRequestedNotAllowlisted :: MText
errSwapRequestedNotAllowlisted = [mt|SWAP_REQUESTED_FA2_NOT_ALLOWLISTED|]

errNoXtzTransferred :: MText 
errNoXtzTransferred = [mt|SWAP_REQUESTED_XTZ_INVALID|]
