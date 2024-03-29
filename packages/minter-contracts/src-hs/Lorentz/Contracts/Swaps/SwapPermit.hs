-- | Lorentz bindings for the swap with permit contract
module Lorentz.Contracts.Swaps.SwapPermit where

import Lorentz

import Lorentz.Contracts.Swaps.Allowlisted
-- Types
----------------------------------------------------------------------------

data Permit = Permit
  { signerKey :: PublicKey
  , signature :: Signature
  }

customGeneric "Permit" ligoCombLayout
deriving anyclass instance IsoValue Permit
deriving anyclass instance HasAnnotation Permit

data OffchainAcceptParam = OffchainAcceptParam
  { swapId :: Natural
  , permit :: Permit
  }

customGeneric "OffchainAcceptParam" ligoCombLayout
deriving anyclass instance IsoValue OffchainAcceptParam
deriving anyclass instance HasAnnotation OffchainAcceptParam

data PermitSwapEntrypoints
  = BaseSwap AllowlistedSwapEntrypoints
  | Offchain_accept [OffchainAcceptParam]

customGeneric "PermitSwapEntrypoints" ligoLayout
deriving anyclass instance IsoValue PermitSwapEntrypoints
deriving anyclass instance HasAnnotation PermitSwapEntrypoints

instance ParameterHasEntrypoints PermitSwapEntrypoints where
  type ParameterEntrypointsDerivation PermitSwapEntrypoints = EpdDelegate

-- Errors
----------------------------------------------------------------------------
