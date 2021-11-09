{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Test.Swaps.Util
  ( originateFA2
  , originateWithAdmin
  , originateSwap
  , originateAllowlistedSwap
  , originateAllowlistedSwapWithAdmin
  , originateOffchainSwap
  , originateOffchainSwapWithAdmin
  , mkFA2Assets
  ) where

import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Value
import qualified Michelson.Typed as T
import Morley.Nettest

import Lorentz.Contracts.Swaps.Allowlisted
import Lorentz.Contracts.Swaps.Basic
import Lorentz.Contracts.Swaps.SwapPermit
import Test.Util

-- | Originate the swaps contract.
originateSwap
  :: MonadNettest caps base m
  => m (TAddress SwapEntrypoints)
originateSwap = do
  TAddress <$> originateUntypedSimple "swaps"
    (T.untypeValue $ T.toVal initSwapStorage)
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

-- | Originate the allowlisted swaps contract and admin for it.
originateOffchainSwapWithAdmin
  :: MonadNettest caps base m
  => m (TAddress PermitSwapEntrypoints, Address)
originateOffchainSwapWithAdmin =
  originateWithAdmin originateOffchainSwap

-- | Construct 'FA2Assets' from a simplified representation.
mkFA2Assets :: TAddress fa2Param -> [(FA2.TokenId, Natural)] -> FA2Assets
mkFA2Assets addr tokens =
  FA2Assets (toAddress addr) (uncurry FA2Token <$> tokens)
