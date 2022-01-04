-- | Lorentz bindings for FA2-related types.
module Lorentz.Contracts.FA2
  ( OperatorStorage
  , OperatorKey(..)
  , TokenMetadata(..)
  , Parameter(..)
  ) where

import Fmt (Buildable(..), genericF)
import Lorentz

import qualified Lorentz.Contracts.Spec.FA2Interface as FA2I

type OperatorStorage = BigMap OperatorKey ()

data OperatorKey = OperatorKey
  { owner :: Address
  , operator :: Address
  , tokenId :: FA2I.TokenId
  }
  deriving stock (Show, Eq, Ord)
customGeneric "OperatorKey" rightComb
deriving anyclass instance IsoValue OperatorKey
deriving anyclass instance HasAnnotation OperatorKey
instance Buildable OperatorKey where build = genericF

-- TODO: add this type to morley-ledgers.
data TokenMetadata = TokenMetadata
  { tokenId :: FA2I.TokenId
  , tokenInfo :: FA2I.TokenMetadata
  }
  deriving stock (Show, Eq)

customGeneric "TokenMetadata" rightComb
deriving anyclass instance IsoValue TokenMetadata
deriving anyclass instance HasAnnotation TokenMetadata

-- | TZIP-12 (and, consequently, @morley-ledgers@) does not prescribe any specific
-- layout for the FA2 parameter.
-- So here, we define a data type that uses the same layout we chose for our ligo contracts.
data Parameter
  = Balance_of FA2I.BalanceRequestParams
  | Transfer FA2I.TransferParams
  | Update_operators FA2I.UpdateOperatorsParam
  deriving stock (Eq, Show)

customGeneric "Parameter" ligoLayout
deriving anyclass instance IsoValue Parameter
deriving anyclass instance HasAnnotation Parameter

instance ParameterHasEntrypoints Parameter where
  type ParameterEntrypointsDerivation Parameter = EpdDelegate
