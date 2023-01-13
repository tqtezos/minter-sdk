-- | Lorentz bindings for the bonding curve contract
module Lorentz.Contracts.BondingCurve where

import Lorentz (Contract, Lambda, Mutez)
import Lorentz.Test.Import (embedContractM)

import Lorentz.Contracts.MinterSdk (inBinFolder)
import Lorentz.Contracts.BondingCurve.Interface (Entrypoints(..), PiecewisePolynomial(..), Storage(..))
import Lorentz.Contracts.BondingCurve.Interface.Debug (DebugEntrypoints(..))

bondingCurveContract :: Contract Entrypoints (Storage (Lambda Natural Mutez))
bondingCurveContract = $$(embedContractM (inBinFolder "bonding_curve.tz"))

debugBondingCurveContract :: Contract DebugEntrypoints (Storage (Lambda Natural Mutez))
debugBondingCurveContract = $$(embedContractM (inBinFolder "bonding_curve_debug.tz"))

bondingCurvePiecewiseContract :: Contract Entrypoints (Storage PiecewisePolynomial)
bondingCurvePiecewiseContract = $$(embedContractM (inBinFolder "bonding_curve_piecewise.tz"))

debugBondingCurvePiecewiseContract :: Contract DebugEntrypoints (Storage PiecewisePolynomial)
debugBondingCurvePiecewiseContract = $$(embedContractM (inBinFolder "bonding_curve_piecewise_debug.tz"))

