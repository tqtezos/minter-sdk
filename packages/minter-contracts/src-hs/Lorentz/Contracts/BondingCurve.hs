-- | Lorentz bindings for the bonding curve contract
module Lorentz.Contracts.BondingCurve where

import Lorentz (Contract)
import Lorentz.Test.Import (embedContractM)

import Lorentz.Contracts.MinterSdk (inBinFolder)
import Lorentz.Contracts.BondingCurve.Interface (Entrypoints(..), Storage(..))
import Lorentz.Contracts.BondingCurve.Interface.Debug (DebugEntrypoints(..))

bondingCurveContract :: Contract Entrypoints Storage
bondingCurveContract = $$(embedContractM (inBinFolder "bonding_curve.tz"))

debugBondingCurveContract :: Contract DebugEntrypoints Storage
debugBondingCurveContract = $$(embedContractM (inBinFolder "bonding_curve_debug.tz"))

