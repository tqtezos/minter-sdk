-- | Lorentz bindings for common types used in all variants of english auction contract.
module Lorentz.Contracts.EnglishAuction.Common where

import Fmt (Buildable(..), genericF)
import Lorentz
import Lorentz.Contracts.Spec.FA2Interface (TokenId)

-- Types
----------------------------------------------------------------------------

newtype AuctionId = AuctionId Natural
  deriving stock (Generic, Eq, Ord, Show)
  deriving newtype (IsoValue, HasAnnotation, Buildable)

data FA2Token = FA2Token
  { tokenId :: TokenId
  , amount :: Natural
  }
  deriving stock (Eq, Show)

customGeneric "FA2Token" ligoCombLayout
deriving anyclass instance IsoValue FA2Token
deriving anyclass instance HasAnnotation FA2Token
instance Buildable FA2Token where build = genericF

data Tokens = Tokens
  { fa2Address :: Address
  , fa2Batch :: [FA2Token]
  }
  deriving stock (Eq, Show)

customGeneric "Tokens" ligoCombLayout
deriving anyclass instance IsoValue Tokens
deriving anyclass instance HasAnnotation Tokens
instance Buildable Tokens where build = genericF
