-- | Allowlisting implementation for restricting addresses and tokens.
--
-- This module is to be imported qualified.
module Lorentz.Contracts.AllowlistToken where

import Lorentz

import qualified Data.Map as Map

import Lorentz.Contracts.Spec.FA2Interface

data AllowedTokenIds
  = AllTokenIdsAllowed
  | TokenIdsAllowed (Set TokenId)

customGeneric "AllowedTokenIds" ligoLayout
deriving anyclass instance IsoValue AllowedTokenIds
deriving anyclass instance HasAnnotation AllowedTokenIds

-- | Allowlists disjunction.
instance Semigroup AllowedTokenIds where
  TokenIdsAllowed s1 <> TokenIdsAllowed s2 = TokenIdsAllowed (s1 <> s2)
  AllTokenIdsAllowed <> _ = AllTokenIdsAllowed
  _ <> AllTokenIdsAllowed = AllTokenIdsAllowed

type Allowlist = BigMap Address AllowedTokenIds

data AllowlistUpdate = AllowlistUpdate
  { toRemove :: Set Address
  , toAdd :: Map Address AllowedTokenIds
  }

customGeneric "AllowlistUpdate" ligoCombLayout
deriving anyclass instance IsoValue AllowlistUpdate
deriving anyclass instance HasAnnotation AllowlistUpdate

-- | Has semantics of sequential application of multiple updates.
instance Monoid AllowlistUpdate where
  mempty = AllowlistUpdate{ toRemove = mempty, toAdd = mempty }
instance Semigroup AllowlistUpdate where
  u1 <> u2 = AllowlistUpdate
    { toRemove = toRemove u1 <> toRemove u2
    , toAdd = Map.unionWith (<>)
        (toAdd u1 `Map.difference` Map.fromSet (\_ -> ()) (toRemove u2))
        (toAdd u2)
    }

type Entrypoints = AllowlistUpdate
