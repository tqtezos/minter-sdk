-- | Lorentz bindings for the allowlisted swaps contract.
module Lorentz.Contracts.AllowlistedSwaps
  ( module Lorentz.Contracts.AllowlistedSwaps
  ) where

import Lorentz

import Lorentz.Contracts.MinterSdk
import Michelson.Test.Import (embedContractM)
import qualified Michelson.Typed as T

import Lorentz.Contracts.NonPausableSimpleAdmin
import Lorentz.Contracts.Swaps

-- Types
----------------------------------------------------------------------------

type Allowlist = BigMap Address ()

data AllowlistedSwapStorage = AllowlistedSwapStorage
  { swapStorage :: SwapStorage
  , admin :: AdminStorage
  , allowlist :: Allowlist
  }

customGeneric "AllowlistedSwapStorage" ligoLayout
deriving anyclass instance IsoValue AllowlistedSwapStorage
deriving anyclass instance HasAnnotation AllowlistedSwapStorage

initAllowlistedSwapStorage :: Address -> AllowlistedSwapStorage
initAllowlistedSwapStorage admin = AllowlistedSwapStorage
  { swapStorage = initSwapStorage
  , admin = initAdminStorage admin
  , allowlist = mempty
  }

data AllowlistedSwapEntrypoints
  = Swap SwapEntrypoints
  | Admin AdminEntrypoints
  | Update_allowed (BigMap Address ())

customGeneric "AllowlistedSwapEntrypoints" ligoLayout
deriving anyclass instance IsoValue AllowlistedSwapEntrypoints
deriving anyclass instance HasAnnotation AllowlistedSwapEntrypoints

instance ParameterHasEntrypoints AllowlistedSwapEntrypoints where
  type ParameterEntrypointsDerivation AllowlistedSwapEntrypoints = EpdDelegate

-- This empty slice is a workaround, so that all the declarations above and
-- their instances may be in the type environment in the TH splice below.
$(pure [])

-- Contract
----------------------------------------------------------------------------

allowlistedSwapsContract
  :: T.Contract (ToT AllowlistedSwapEntrypoints) (ToT AllowlistedSwapStorage)
allowlistedSwapsContract =
  $$(embedContractM (inBinFolder "fa2_allowlisted_swap.tz"))

-- Errors
----------------------------------------------------------------------------

errSwapOfferedNotAllowlisted :: MText
errSwapOfferedNotAllowlisted = [mt|SWAP_OFFERED_FA2_NOT_ALLOWLISTED|]

errSwapRequestedNotAllowlisted :: MText
errSwapRequestedNotAllowlisted = [mt|SWAP_REQUESTED_FA2_NOT_ALLOWLISTED|]
