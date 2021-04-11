{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Test.Swaps.Util
  ( originateFA2
  , originateWithAdmin
  , originateSwap
  , originateAllowlistedSwap
  , originateAllowlistedSwapWithAdmin
  , mkFA2Assets
  ) where

import qualified Data.Foldable as F
import qualified Data.Map as Map

import qualified Indigo.Contracts.FA2Sample as FA2
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Value
import qualified Michelson.Typed as T
import Morley.Nettest

import Lorentz.Contracts.Swaps.Allowlisted
import Lorentz.Contracts.Swaps.Basic
import Test.Util

-- | Originate a trivial FA2 contract suitable for testing the provided swaps
-- contract:
-- * Some money will be put on the addresses from setup, the swaps contract
--   will be made operator of those addresses.
-- * The tokenIds from setup will be supported by the originated contract.
originateFA2
  :: MonadNettest caps base m
  => AliasHint
  -> FA2Setup addrsNum tokensNum
  -> TAddress swapParam
  -> m (TAddress FA2.FA2SampleParameter)
originateFA2 name FA2Setup{..} swapContract = do
  fa2 <- originateSimple name
    FA2.Storage
    { sLedger = BigMap $ Map.fromList do
        -- put money on several tokenIds for each given address
        addr <- F.toList sAddresses
        tokenId <- F.toList sTokens
        pure ((addr, tokenId), 1000)
    , sOperators = BigMap $ Map.fromList do
        addr <- F.toList sAddresses
        pure ((addr, toAddress swapContract), ())
    , sTokenMetadata = mempty
    }
    (FA2.fa2Contract def
      { FA2.cAllowedTokenIds = F.toList sTokens
      }
    )
  return fa2

-- | Originate the swaps contract.
originateSwap
  :: MonadNettest caps base m
  => m (TAddress SwapEntrypoints)
originateSwap = do
  TAddress <$> originateUntypedSimple "swaps"
    (T.untypeValue $ T.toVal initSwapStorage)
    (T.convertContract swapsContract)

-- | Originate the a contract and admin for it.
originateWithAdmin
  :: MonadNettest caps base m
  => (Address -> m (TAddress param))
  -> m (TAddress param, Address)
originateWithAdmin originateFn = do
  admin <- newAddress "admin"
  swaps <- originateFn admin
  return (swaps, admin)

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

-- | Construct 'FA2Assets' from a simplified representation.
mkFA2Assets :: TAddress fa2Param -> [(FA2.TokenId, Natural)] -> FA2Assets
mkFA2Assets addr tokens =
  FA2Assets (toAddress addr) (uncurry FA2Token <$> tokens)
