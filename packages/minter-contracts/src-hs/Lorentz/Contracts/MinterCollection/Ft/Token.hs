-- | Lorentz bindings for @minter_collection/ft/fa2_multi_ft_token.mligo@
module Lorentz.Contracts.MinterCollection.Ft.Token
  ( Storage(..)
  , LimitedStorage(..)
  , LimitedStorageWithContractOperators(..)
  ) where

import Lorentz

import qualified Lorentz.Contracts.FA2 as FA2
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2I
import Util ()

data Storage = Storage
  { ledger :: BigMap (Address, FA2I.TokenId) Natural
  , operators :: FA2.OperatorStorage
  , tokenMetadata :: BigMap FA2I.TokenId FA2.TokenMetadata 
  , totalTokenSupply :: BigMap FA2I.TokenId Natural 
  }
  deriving stock (Show, Eq)
customGeneric "Storage" ligoLayout
deriving anyclass instance IsoValue Storage
deriving anyclass instance HasAnnotation Storage

data LimitedStorage = LimitedStorage
  { ledger :: BigMap (Address, FA2I.TokenId) Natural
  , operators :: FA2.OperatorStorage
  , tokenMetadata :: BigMap FA2I.TokenId FA2.TokenMetadata 
  , totalTokenSupply :: BigMap FA2I.TokenId Natural 
  , nextTokenId :: Natural
  }
  deriving stock (Show, Eq)
customGeneric "LimitedStorage" ligoLayout
deriving anyclass instance IsoValue LimitedStorage
deriving anyclass instance HasAnnotation LimitedStorage

data LimitedStorageWithContractOperators = LimitedStorageWithContractOperators
  { ledger :: BigMap (Address, FA2I.TokenId) Natural
  , operators :: FA2.OperatorStorage
  , nextTokenId :: Natural
  , tokenMetadata :: BigMap FA2I.TokenId FA2.TokenMetadata 
  , totalTokenSupply :: BigMap FA2I.TokenId Natural 
  , contractOperators :: Set Address
  }
  deriving stock (Show, Eq)
customGeneric "LimitedStorageWithContractOperators" ligoLayout
deriving anyclass instance IsoValue LimitedStorageWithContractOperators
deriving anyclass instance HasAnnotation LimitedStorageWithContractOperators
