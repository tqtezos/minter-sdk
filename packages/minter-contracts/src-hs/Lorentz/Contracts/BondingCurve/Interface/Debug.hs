-- | Lorentz interface for the bonding curve contract (debug)
module Lorentz.Contracts.BondingCurve.Interface.Debug where

import Lorentz

import Lorentz.Contracts.BondingCurve.Interface ()
import Lorentz.Contracts.SimpleAdmin (AdminEntrypoints(..))
import Lorentz.Contracts.Spec.FA2Interface (TokenId)

-- Same as bonding curve entrypoints, but GetCost
data DebugEntrypoints
  = Admin AdminEntrypoints
  | Set_delegate (Maybe KeyHash)
  | Withdraw ()
  | Buy ()
  | Buy_offchain Address
  | Sell TokenId
  | Sell_offchain (TokenId, Address)

  -- | Get the current cost (debug only)
  | Cost Natural

  -- | Get the Michelson implementation of (x ^ n) for (x, n) input (debug only)
  | Pow (Natural, Natural)

  -- | Get the Michelson implementation of the example formula 0 (debug only)
  | ExampleFormula0 Natural
  deriving stock (Eq, Show)

customGeneric "DebugEntrypoints" ligoLayout
deriving anyclass instance IsoValue DebugEntrypoints
deriving anyclass instance HasAnnotation DebugEntrypoints

instance ParameterHasEntrypoints DebugEntrypoints where
  -- EpdRecursive so that AdminEntrypoints are reached
  type ParameterEntrypointsDerivation DebugEntrypoints = EpdRecursive

