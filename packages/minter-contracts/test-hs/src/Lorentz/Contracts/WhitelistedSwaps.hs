-- | Lorentz bindings for the swaps contract.
module Lorentz.Contracts.WhitelistedSwaps
  ( module Lorentz.Contracts.WhitelistedSwaps
  ) where

import Lorentz

import Lorentz.Contracts.MinterSdk
import Michelson.Test.Import (embedContractM)
import qualified Michelson.Typed as T

import Lorentz.Contracts.NonPausableSimpleAdmin
import Lorentz.Contracts.Swaps

-- Types
----------------------------------------------------------------------------

type Whitelist = BigMap Address ()

data WhitelistedSwapStorage = WhitelistedSwapStorage
  { swapStorage :: SwapStorage
  , admin :: AdminStorage
  , whitelist :: Whitelist
  }

customGeneric "WhitelistedSwapStorage" ligoLayout
deriving anyclass instance IsoValue WhitelistedSwapStorage
deriving anyclass instance HasAnnotation WhitelistedSwapStorage

initWhitelistedSwapStorage :: Address -> WhitelistedSwapStorage
initWhitelistedSwapStorage admin = WhitelistedSwapStorage
  { swapStorage = initSwapStorage
  , admin = initAdminStorage admin
  , whitelist = mempty
  }

data WhitelistedSwapEntrypoints
  = Swap SwapEntrypoints
  | Admin AdminEntrypoints
  | Update_allowed (BigMap Address ())

customGeneric "WhitelistedSwapEntrypoints" ligoLayout
deriving anyclass instance IsoValue WhitelistedSwapEntrypoints
deriving anyclass instance HasAnnotation WhitelistedSwapEntrypoints

instance ParameterHasEntrypoints WhitelistedSwapEntrypoints where
  type ParameterEntrypointsDerivation WhitelistedSwapEntrypoints = EpdDelegate

-- This empty slice is a workaround, so that all the declarations above and
-- their instances may be in the type environment in the TH splice below.
$(pure [])

-- Contract
----------------------------------------------------------------------------

whitelistedSwapsContract
  :: T.Contract (ToT WhitelistedSwapEntrypoints) (ToT WhitelistedSwapStorage)
whitelistedSwapsContract =
  $$(embedContractM (inBinFolder "fa2_whitelisted_swap.tz"))

-- Errors
----------------------------------------------------------------------------

errSwapOfferedNotWhitelisted :: MText
errSwapOfferedNotWhitelisted = [mt|SWAP_OFFERED_FA2_NOT_WHITELISTED|]

errSwapRequestedNotWhitelisted :: MText
errSwapRequestedNotWhitelisted = [mt|SWAP_REQUESTED_FA2_NOT_WHITELISTED|]
