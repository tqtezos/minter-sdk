-- | Lorentz interface for the bonding curve contract (debug)
module Lorentz.Contracts.BondingCurve.Interface.Debug where

import Lorentz

import Lorentz.Contracts.BondingCurve.Interface ()
import Lorentz.Contracts.SimpleAdmin (AdminEntrypoints(..))
import Lorentz.Contracts.Spec.FA2Interface (TokenId)

-- Same as bonding curve entrypoints, but GetCost
data DebugEntrypoints
  = Admin AdminEntrypoints
  | SetDelegate (Maybe KeyHash)
  | Withdraw ()
  | Buy ()
  | BuyOffchain Address
  | Sell TokenId
  | SellOffchain (TokenId, Address)

  -- | Get the current cost (debug only)
  | Cost Natural
  deriving stock (Eq, Show)

customGeneric "DebugEntrypoints" ligoLayout
deriving anyclass instance IsoValue DebugEntrypoints
deriving anyclass instance HasAnnotation DebugEntrypoints

instance ParameterHasEntrypoints DebugEntrypoints where
  -- EpdRecursive so that AdminEntrypoints are reached
  type ParameterEntrypointsDerivation DebugEntrypoints = EpdRecursive

