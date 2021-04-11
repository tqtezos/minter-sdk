{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Test.Swaps.Util
  ( Setup (..)
  , doSetup
  , originateFA2
  , originateSwap
  , originateAllowlistedSwap
  , originateAllowlistedSwapWithAdmin
  , mkFA2Assets
  , mkAllowlistParam
  ) where

import qualified Data.Foldable as F
import qualified Data.Map as Map
import qualified Data.Sized as Sized
import Data.Type.Ordinal (ordToNatural)

import qualified Indigo.Contracts.FA2Sample as FA2
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Value
import qualified Michelson.Typed as T
import Morley.Nettest

import Lorentz.Contracts.Swaps.Allowlisted
import Lorentz.Contracts.Swaps.Basic
import Test.Util

-- | Test setup.
--
-- We remember sizes of all entries lists in types because that facilitates
-- allocating exactly as many entities as necessary for the given test suite.
data Setup addrsNum tokensNum = Setup
  { sAddresses :: Sized [] addrsNum Address
  , sTokens :: Sized [] tokensNum FA2.TokenId
  } deriving stock (Show)

{- | Prepare all the operated entities.

Note that number of addresses and other entities may be inferred automatically,
so you should bind all the fields of returned 'Setup'. For instance:

@
scenario = do
  setup <- doSetup
  let addr1 ::< addr2 ::< SNil = sAddresses setup
  -- ↑ Here compiler figures out that exactly 2 addresses should be allocated
  -- during setup...

  let tokenId :< Nil = sTokens setup
  -- ↑ ...and only one token.

  ...
@

Another option is to explicitly annotate the 'doSetup' call:

@
scenario = do
  setup <- toSetup @("addresses" :# 2) @("tokens" :# 1)
  ...
@
-}
doSetup
  :: forall addrsArg tokensArg addrsNum tokensNum caps base m.
     ( MonadNettest caps base m
     , KnownNat addrsNum, addrsArg ~ ("addresses" :# addrsNum)
     , KnownNat tokensNum, tokensArg ~ ("tokens" :# tokensNum)
     )
  => m (Setup addrsNum tokensNum)
doSetup = do
  let aliases = Sized.generate' $ \i -> fromString ("fa2-addr-" <> show (ordToNatural i))
  sAddresses <- mapM newAddress aliases
  let sTokens = Sized.generate' $ \i -> FA2.TokenId (ordToNatural i)
  return Setup{..}

-- | Originate a trivial FA2 contract suitable for testing the provided swaps
-- contract:
-- * Some money will be put on the addresses from setup, the swaps contract
--   will be made operator of those addresses.
-- * The tokenIds from setup will be supported by the originated contract.
originateFA2
  :: MonadNettest caps base m
  => AliasHint
  -> Setup addrsNum tokensNum
  -> TAddress swapParam
  -> m (TAddress FA2.FA2SampleParameter)
originateFA2 name Setup{..} swapContract = do
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
originateAllowlistedSwapWithAdmin = do
  admin <- newAddress "swaps-admin"
  swaps <- originateAllowlistedSwap admin
  return (swaps, admin)

-- | Construct 'FA2Assets' from a simplified representation.
mkFA2Assets :: TAddress fa2Param -> [(FA2.TokenId, Natural)] -> FA2Assets
mkFA2Assets addr tokens =
  FA2Assets (toAddress addr) (uncurry FA2Token <$> tokens)

-- | Construct allowlist for passing to allowlist overriding entrypoint.
mkAllowlistParam :: [Address] -> BigMap Address ()
mkAllowlistParam = mconcat . map (\a -> one (a, ()))
