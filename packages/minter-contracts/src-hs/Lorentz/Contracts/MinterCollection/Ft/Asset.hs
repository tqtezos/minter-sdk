-- | Lorentz bindings for @minter_collection/ft/fa2_multi_ft_asset.mligo@
module Lorentz.Contracts.MinterCollection.Ft.Asset
  ( LimitedWithGlobalOperatorsEntrypoints(..)
  , LimitedStorageWithGlobalOperators(..)
  , limitedWithGlobalOperatorsContract
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

data LimitedWithGlobalOperatorsEntrypoints
  = Assets FA2.Parameter
  | Admin Admin.AdminEntrypoints
  | Update_global_operators (Set Address)
  | Mint [MintLimitedParam]
  deriving stock (Show, Eq)
customGeneric "LimitedWithGlobalOperatorsEntrypoints" ligoLayout
deriving anyclass instance IsoValue LimitedWithGlobalOperatorsEntrypoints
deriving anyclass instance HasAnnotation LimitedWithGlobalOperatorsEntrypoints

instance ParameterHasEntrypoints LimitedWithGlobalOperatorsEntrypoints where
  type ParameterEntrypointsDerivation LimitedWithGlobalOperatorsEntrypoints = EpdDelegate

-- Note: Hardcoded to use 'PausableAdminOption' (Simple admin),
-- but should be generalized to work with any admin.
data LimitedStorageWithGlobalOperators = LimitedStorageWithGlobalOperators
  { assets :: FtToken.LimitedStorageWithGlobalOperators
  , admin :: Admin.AdminStorageRecord
  , metadata :: BigMap MText ByteString
  }
  deriving stock (Show, Eq)
customGeneric "LimitedStorageWithGlobalOperators" ligoLayout
deriving anyclass instance IsoValue LimitedStorageWithGlobalOperators
deriving anyclass instance HasAnnotation LimitedStorageWithGlobalOperators

limitedWithGlobalOperatorsContract :: T.Contract (ToT LimitedWithGlobalOperatorsEntrypoints) (ToT LimitedStorageWithGlobalOperators)
limitedWithGlobalOperatorsContract = $$(embedContractM (inBinFolder "fa2_multi_ft_asset_limited_contract_operator.tz"))