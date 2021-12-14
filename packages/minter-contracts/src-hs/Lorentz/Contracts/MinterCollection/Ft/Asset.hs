-- | Lorentz bindings for @minter_collection/ft/fa2_multi_ft_asset.mligo@
module Lorentz.Contracts.MinterCollection.Ft.Asset
  ( LimitedWithContractOperatorsEntrypoints(..)
  , LimitedStorageWithContractOperators(..)
  , limitedWithContractOperatorsContract
  ) where

import Lorentz

import qualified Lorentz.Contracts.FA2 as FA2
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2I
import qualified Lorentz.Contracts.MinterCollection.Ft.Token as FtToken
import qualified Lorentz.Contracts.PausableAdminOption as Admin

import qualified Michelson.Typed as T
import Michelson.Test.Import (embedContractM)

import Lorentz.Contracts.MinterSdk

data MintLimitedParam = MintLimitedParam 
  { owner :: Address
  , amount :: Natural 
  , tokenInfo :: FA2I.TokenMetadata 
  }
  deriving stock (Show, Eq)
customGeneric "MintLimitedParam" rightComb
deriving anyclass instance IsoValue MintLimitedParam
deriving anyclass instance HasAnnotation MintLimitedParam

data LimitedWithContractOperatorsEntrypoints
  = Assets FA2.Parameter
  | Admin Admin.AdminEntrypoints
  | Update_contract_operators (Set Address)
  | Mint [MintLimitedParam]
  deriving stock (Show, Eq)
customGeneric "LimitedWithContractOperatorsEntrypoints" ligoLayout
deriving anyclass instance IsoValue LimitedWithContractOperatorsEntrypoints
deriving anyclass instance HasAnnotation LimitedWithContractOperatorsEntrypoints

-- Note: Hardcoded to use 'PausableAdminOption' (Simple admin),
-- but should be generalized to work with any admin.
data LimitedStorageWithContractOperators = LimitedStorageWithContractOperators
  { assets :: FtToken.LimitedStorageWithContractOperators
  , admin :: Admin.AdminStorageRecord
  , metadata :: BigMap MText ByteString
  }
  deriving stock (Show, Eq)
customGeneric "LimitedStorageWithContractOperators" ligoLayout
deriving anyclass instance IsoValue LimitedStorageWithContractOperators
deriving anyclass instance HasAnnotation LimitedStorageWithContractOperators

limitedWithContractOperatorsContract :: T.Contract (ToT LimitedWithContractOperatorsEntrypoints) (ToT LimitedStorageWithContractOperators)
limitedWithContractOperatorsContract = $$(embedContractM (inBinFolder "fa2_multi_ft_asset_limited_contract_operator.tz"))