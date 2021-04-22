-- | Allowlisting implementation for restricting addresses and tokens.
--
-- This module is to be imported qualified.
module Lorentz.Contracts.AllowlistToken where

import Lorentz

import Lorentz.Contracts.Spec.FA2Interface

data AllowedTokenIds
  = AllTokenIdsAllowed
  | TokenIdsAllowed (Set TokenId)

customGeneric "AllowedTokenIds" ligoLayout
deriving anyclass instance IsoValue AllowedTokenIds
deriving anyclass instance HasAnnotation AllowedTokenIds

type Allowlist = BigMap Address AllowedTokenIds

data AllowlistUpdate = AllowlistUpdate
  { toRemove :: Set Address
  , toAdd :: Map Address AllowedTokenIds
  }

customGeneric "AllowlistUpdate" ligoCombLayout
deriving anyclass instance IsoValue AllowlistUpdate
deriving anyclass instance HasAnnotation AllowlistUpdate

type Entrypoints = AllowlistUpdate
