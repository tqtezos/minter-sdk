-- | Lorentz swap contracts.
module Lorentz.Contracts.Swaps.Contracts where

import Lorentz
import Lorentz.Test.Import (embedContractM)

import Lorentz.Contracts.MinterSdk

import Lorentz.Contracts.Swaps.Allowlisted
import Lorentz.Contracts.Swaps.Basic
import Lorentz.Contracts.Swaps.SwapPermit

import qualified Lorentz.Contracts.Swaps.AllowlistedFee as AllowlistedFee
import Lorentz.Contracts.Swaps.Burn

import Lorentz.Contracts.Swaps.Collections
import Lorentz.Contracts.Swaps.SwapPermitBurnFee

swapsContract :: Contract SwapEntrypoints SwapStorage
swapsContract =
  $$(embedContractM (inBinFolder "fa2_swap.tz"))

allowlistedSwapsContract
  :: Contract AllowlistedSwapEntrypoints AllowlistedSwapStorage
allowlistedSwapsContract =
  $$(embedContractM (inBinFolder "fa2_allowlisted_swap.tz"))

allowlistedBurnSwapsContract
  :: Contract AllowlistedBurnSwapEntrypoints AllowlistedBurnSwapStorage
allowlistedBurnSwapsContract =
  $$(embedContractM (inBinFolder "fa2_allowlisted_swap_with_burn.tz"))

changeBurnAddressSwapsContract
  :: Contract ChangeBurnAddressSwapEntrypoints AllowlistedBurnSwapStorage
changeBurnAddressSwapsContract =
  $$(embedContractM (inBinFolder "fa2_allowlisted_swap_with_change_burn_address.tz"))

allowlistedSwapsPermitContract
  :: Contract PermitSwapEntrypoints AllowlistedSwapStorage
allowlistedSwapsPermitContract =
  $$(embedContractM (inBinFolder "fa2_allowlisted_swap_offchain_nocounter.tz"))

allowlistedFeeSwapsContract
  :: Contract
       AllowlistedFee.AllowlistedFeeSwapEntrypoints
       AllowlistedFee.AllowlistedFeeSwapStorage
allowlistedFeeSwapsContract =
  $$(embedContractM (inBinFolder "fa2_fee_allowlisted_swap.tz"))

allowlistedSwapsPermitBurnFeeContract
  :: Contract PermitSwapBurnFeeEntrypoints AllowlistedBurnSwapFeeStorage
allowlistedSwapsPermitBurnFeeContract =
  $$(embedContractM (inBinFolder "fa2_swap_offchain_burn_fee.tz"))

collectionsContract
  :: Contract CollectionsEntrypoints CollectionsStorage
collectionsContract =
  $$(embedContractM (inBinFolder "fa2_swap_with_collections_and_burn.tz"))

offchainCollectionsContract
  :: Contract OffchainCollectionsEntrypoints CollectionsStorage
offchainCollectionsContract =
  $$(embedContractM (inBinFolder "fa2_swap_with_collections_and_burn_offchain.tz"))
