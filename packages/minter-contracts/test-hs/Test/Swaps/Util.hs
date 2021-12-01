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
import qualified Michelson.Typed as T
import Morley.Nettest

import Lorentz.Contracts.Swaps.Allowlisted
import Lorentz.Contracts.Swaps.Basic
import Lorentz.Contracts.Swaps.SwapPermit

import Lorentz.Contracts.Swaps.Burn
import Lorentz.Contracts.Swaps.AllowlistedFee

import Lorentz.Contracts.Swaps.Collections
import Lorentz.Contracts.Swaps.SwapPermitBurnFee
import Test.Util

-- | Originate the swaps contract.
originateSwap
  :: MonadNettest caps base m
  => m (TAddress Lorentz.Contracts.Swaps.Basic.SwapEntrypoints)
originateSwap = do
  TAddress <$> originateUntypedSimple "swaps"
    (T.untypeValue $ T.toVal Lorentz.Contracts.Swaps.Basic.initSwapStorage)
    (T.convertContract swapsContract)

-- | Originate the allowlisted swaps contract.
originateAllowlistedSwap
  :: MonadNettest caps base m
  => Address
  -> m (TAddress AllowlistedSwapEntrypoints)
originateAllowlistedSwap admin = do
  TAddress <$> originateUntypedSimple "swaps"
    (T.untypeValue $ T.toVal $ initAllowlistedSwapStorage admin)
    (T.convertContract allowlistedSwapsContract)

-- | Originate the allowlisted burn swaps contract.
originateAllowlistedBurnSwap
  :: MonadNettest caps base m
  => Address
  -> m (TAddress AllowlistedBurnSwapEntrypoints)
originateAllowlistedBurnSwap admin = do
  TAddress <$> originateUntypedSimple "swaps"
    (T.untypeValue $ T.toVal $ initAllowlistedBurnSwapStorage admin)
    (T.convertContract allowlistedBurnSwapsContract)

-- | Originate the allowlisted burn swaps contract and admin for it.
originateAllowlistedBurnSwapWithAdmin
  :: MonadNettest caps base m
  => m (TAddress AllowlistedBurnSwapEntrypoints, Address)
originateAllowlistedBurnSwapWithAdmin =
  originateWithAdmin originateAllowlistedBurnSwap


-- | Originate the allowlisted burn swaps contract.
originateChangeBurnAddressSwap
  :: MonadNettest caps base m
  => Address
  -> m (TAddress ChangeBurnAddressSwapEntrypoints)
originateChangeBurnAddressSwap admin = do
  TAddress <$> originateUntypedSimple "swaps"
    (T.untypeValue $ T.toVal $ initAllowlistedBurnSwapStorage admin)
    (T.convertContract changeBurnAddressSwapsContract)

-- | Originate the allowlisted burn swaps contract and admin for it.
originateChangeBurnAddressSwapWithAdmin
  :: MonadNettest caps base m
  => m (TAddress ChangeBurnAddressSwapEntrypoints, Address)
originateChangeBurnAddressSwapWithAdmin =
  originateWithAdmin originateChangeBurnAddressSwap


-- | Originate the allowlisted burn swaps contract and admin for it.
originateOffchainSwapBurnFeeWithAdmin
  :: MonadNettest caps base m
  => m (TAddress PermitSwapBurnFeeEntrypoints, Address)
originateOffchainSwapBurnFeeWithAdmin =
  originateWithAdmin originateOffchainSwapBurnFee


-- | Originate the allowlisted feeswaps contract with tez fee.
originateAllowlistedFeeSwap
  :: MonadNettest caps base m
  => Address
  -> m (TAddress Lorentz.Contracts.Swaps.AllowlistedFee.AllowlistedFeeSwapEntrypoints)
originateAllowlistedFeeSwap admin = do
  TAddress <$> originateUntypedSimple "swaps"
    (T.untypeValue $ T.toVal $ initAllowlistedFeeSwapStorage admin)
    (T.convertContract allowlistedFeeSwapsContract)

-- | Originate the allowlisted swaps contract and admin for it.
originateAllowlistedSwapWithAdmin
  :: MonadNettest caps base m
  => m (TAddress AllowlistedSwapEntrypoints, Address)
originateAllowlistedSwapWithAdmin =
  originateWithAdmin originateAllowlistedSwap

-- | Originate the swaps contract with offchain_accept entrypoint.
originateOffchainSwap
  :: MonadNettest caps base m
  => Address
  -> m (TAddress PermitSwapEntrypoints)
originateOffchainSwap admin = do
  TAddress <$> originateUntypedSimple "swaps"
    (T.untypeValue $ T.toVal $ initAllowlistedSwapStorage admin)
    (T.convertContract allowlistedSwapsPermitContract)

-- | Originate the offchain collections contract
originateOffchainCollections
  :: MonadNettest caps base m
  => Address
  -> Address
  -> m (TAddress OffchainCollectionsEntrypoints)
originateOffchainCollections admin fa2 = do
  TAddress <$> originateUntypedSimple "swaps"
    (T.untypeValue $ T.toVal $ initCollectionsStorage admin fa2)
    (T.convertContract offchainCollectionsContract)


-- | Originate the swaps contract with offchain_accept entrypoint.
originateOffchainSwapBurnFee
  :: MonadNettest caps base m
  => Address
  -> m (TAddress PermitSwapBurnFeeEntrypoints)
originateOffchainSwapBurnFee admin = do
  TAddress <$> originateUntypedSimple "swaps"
    (T.untypeValue $ T.toVal $ initAllowlistedBurnSwapFeeStorage admin)
    (T.convertContract allowlistedSwapsPermitBurnFeeContract)

-- | Originate the allowlisted swaps contract and admin for it.
originateOffchainSwapWithAdmin
  :: MonadNettest caps base m
  => m (TAddress PermitSwapEntrypoints, Address)
originateOffchainSwapWithAdmin =
  originateWithAdmin originateOffchainSwap

---- | Originate the offchain collections contract
--originateOffchainCollectionsWithAdmin
--  :: MonadNettest caps base m
--  => m (TAddress OffchainCollectionsEntrypoints, Address)
--originateOffchainCollectionsWithAdmin = 
--  originateWithAdmin originateOffchainCollections

-- | Construct 'FA2Assets' from a simplified representation.
mkFA2Assets :: TAddress fa2Param -> [(FA2.TokenId, Natural)] -> FA2Assets
mkFA2Assets addr tokens =
  FA2Assets (toAddress addr) (uncurry FA2Token <$> tokens)