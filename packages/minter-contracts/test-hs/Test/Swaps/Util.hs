{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Test.Swaps.Util
  ( originateFA2
  , originateWithAdmin
  , originateSwap
  , originateAllowlistedSwap
  , originateAllowlistedBurnSwap
  , originateAllowlistedFeeSwap
  , originateChangeBurnAddressSwap
  , originateOffchainCollections
  , originateOffchainSwap
  , originateOffchainSwapBurnFee
  --, originateOffchainCollectionsWithAdmin
  , originateAllowlistedSwapWithAdmin
  , originateOffchainSwapWithAdmin
  , originateAllowlistedBurnSwapWithAdmin
  , originateChangeBurnAddressSwapWithAdmin
  , originateOffchainSwapBurnFeeWithAdmin
  , mkFA2Assets
  ) where

import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Value
import Morley.Nettest

import Lorentz.Contracts.Swaps.Contracts

import Lorentz.Contracts.Swaps.Allowlisted
import Lorentz.Contracts.Swaps.Basic
import Lorentz.Contracts.Swaps.SwapPermit

import qualified Lorentz.Contracts.Swaps.AllowlistedFee as AllowlistedFee
import Lorentz.Contracts.Swaps.Burn

import Lorentz.Contracts.Swaps.Collections
import Lorentz.Contracts.Swaps.SwapPermitBurnFee

import Test.Util

-- | Originate the swaps contract.
originateSwap
  :: MonadNettest caps base m
  => m (ContractHandler SwapEntrypoints SwapStorage)
originateSwap =
  originateSimple "swaps" initSwapStorage swapsContract

-- | Originate the allowlisted swaps contract.
originateAllowlistedSwap
  :: MonadNettest caps base m
  => Address
  -> m (ContractHandler AllowlistedSwapEntrypoints AllowlistedSwapStorage)
originateAllowlistedSwap admin =
  originateSimple "swaps" (initAllowlistedSwapStorage admin) allowlistedSwapsContract

-- | Originate the allowlisted burn swaps contract.
originateAllowlistedBurnSwap
  :: MonadNettest caps base m
  => Address
  -> m (ContractHandler AllowlistedBurnSwapEntrypoints AllowlistedBurnSwapStorage)
originateAllowlistedBurnSwap admin = do
  originateSimple "swaps"
    (initAllowlistedBurnSwapStorage admin)
    allowlistedBurnSwapsContract

-- | Originate the allowlisted burn swaps contract and admin for it.
originateAllowlistedBurnSwapWithAdmin
  :: MonadNettest caps base m
  => m (ContractHandler AllowlistedBurnSwapEntrypoints AllowlistedBurnSwapStorage, Address)
originateAllowlistedBurnSwapWithAdmin =
  originateWithAdmin originateAllowlistedBurnSwap


-- | Originate the allowlisted burn swaps contract.
originateChangeBurnAddressSwap
  :: MonadNettest caps base m
  => Address
  -> m (ContractHandler ChangeBurnAddressSwapEntrypoints AllowlistedBurnSwapStorage)
originateChangeBurnAddressSwap admin = do
  originateSimple "swaps"
    (initAllowlistedBurnSwapStorage admin)
    changeBurnAddressSwapsContract

-- | Originate the allowlisted burn swaps contract and admin for it.
originateChangeBurnAddressSwapWithAdmin
  :: MonadNettest caps base m
  => m (ContractHandler ChangeBurnAddressSwapEntrypoints AllowlistedBurnSwapStorage, Address)
originateChangeBurnAddressSwapWithAdmin =
  originateWithAdmin originateChangeBurnAddressSwap


-- | Originate the allowlisted burn swaps contract and admin for it.
originateOffchainSwapBurnFeeWithAdmin
  :: MonadNettest caps base m
  => m (ContractHandler PermitSwapBurnFeeEntrypoints AllowlistedBurnSwapFeeStorage, Address)
originateOffchainSwapBurnFeeWithAdmin =
  originateWithAdmin originateOffchainSwapBurnFee


-- | Originate the allowlisted feeswaps contract with tez fee.
originateAllowlistedFeeSwap
  :: MonadNettest caps base m
  => Address
  -> m $ ContractHandler
      AllowlistedFee.AllowlistedFeeSwapEntrypoints
      AllowlistedFee.AllowlistedFeeSwapStorage
originateAllowlistedFeeSwap admin = do
  originateSimple "swaps"
    (AllowlistedFee.initAllowlistedFeeSwapStorage admin)
    allowlistedFeeSwapsContract

-- | Originate the allowlisted swaps contract and admin for it.
originateAllowlistedSwapWithAdmin
  :: MonadNettest caps base m
  => m (ContractHandler AllowlistedSwapEntrypoints AllowlistedSwapStorage, Address)
originateAllowlistedSwapWithAdmin =
  originateWithAdmin originateAllowlistedSwap

-- | Originate the swaps contract with offchain_accept entrypoint.
originateOffchainSwap
  :: MonadNettest caps base m
  => Address
  -> m (ContractHandler PermitSwapEntrypoints AllowlistedSwapStorage)
originateOffchainSwap admin = do
  originateSimple "swaps"
    (initAllowlistedSwapStorage admin)
    allowlistedSwapsPermitContract

-- | Originate the offchain collections contract
originateOffchainCollections
  :: MonadNettest caps base m
  => Address
  -> Address
  -> m (ContractHandler OffchainCollectionsEntrypoints CollectionsStorage)
originateOffchainCollections admin fa2 = do
  originateSimple "swaps"
    (initCollectionsStorage admin fa2)
    offchainCollectionsContract


-- | Originate the swaps contract with offchain_accept entrypoint.
originateOffchainSwapBurnFee
  :: MonadNettest caps base m
  => Address
  -> m (ContractHandler PermitSwapBurnFeeEntrypoints AllowlistedBurnSwapFeeStorage)
originateOffchainSwapBurnFee admin = do
  originateSimple "swaps"
    (initAllowlistedBurnSwapFeeStorage admin)
    allowlistedSwapsPermitBurnFeeContract

-- | Originate the allowlisted swaps contract and admin for it.
originateOffchainSwapWithAdmin
  :: MonadNettest caps base m
  => m (ContractHandler PermitSwapEntrypoints AllowlistedSwapStorage, Address)
originateOffchainSwapWithAdmin =
  originateWithAdmin originateOffchainSwap

---- | Originate the offchain collections contract
--originateOffchainCollectionsWithAdmin
--  :: MonadNettest caps base m
--  => m (ContractHandler OffchainCollectionsEntrypoints CollectionsStorage, Address)
--originateOffchainCollectionsWithAdmin =
--  originateWithAdmin originateOffchainCollections

-- | Construct 'FA2Assets' from a simplified representation.
mkFA2Assets :: ContractHandler fa2Param fa2Storage -> [(FA2.TokenId, Natural)] -> FA2Assets
mkFA2Assets addr tokens =
  FA2Assets (toAddress addr) (uncurry FA2Token <$> tokens)
