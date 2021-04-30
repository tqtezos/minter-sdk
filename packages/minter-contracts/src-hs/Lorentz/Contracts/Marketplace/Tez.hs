-- | Lorentz bindings for the fixed price sale contract (Tez version).
module Lorentz.Contracts.Marketplace.Tez where

import Lorentz

import Lorentz.Contracts.MinterSdk
import Lorentz.Contracts.PausableAdminOption
import Lorentz.Contracts.Spec.FA2Interface
import Michelson.Test.Import (embedContractM)
import qualified Michelson.Typed as T

-- Types
----------------------------------------------------------------------------

data SaleTokenParamTez = SaleTokenParamTez
  { tokenForSaleAddress :: Address
  , tokenForSaleTokenId :: TokenId
  } deriving stock (Eq, Ord)

customGeneric "SaleTokenParamTez" ligoCombLayout
deriving anyclass instance IsoValue SaleTokenParamTez
deriving anyclass instance HasAnnotation SaleTokenParamTez

data SaleParamTez = SaleParamTez
  { saleSeller :: Address
  , tokens :: SaleTokenParamTez
  } deriving stock (Eq, Ord)

customGeneric "SaleParamTez" ligoCombLayout
deriving anyclass instance IsoValue SaleParamTez
deriving anyclass instance HasAnnotation SaleParamTez

data InitSaleParamTez = InitSaleParamTez
  { salePrice :: Mutez
  , saleTokensParam :: SaleTokenParamTez
  }

customGeneric "InitSaleParamTez" ligoCombLayout
deriving anyclass instance IsoValue InitSaleParamTez
deriving anyclass instance HasAnnotation InitSaleParamTez

data MarketplaceTezStorage = MarketplaceTezStorage
  { admin :: AdminStorage
  , sales :: BigMap SaleParamTez Mutez
  }

customGeneric "MarketplaceTezStorage" ligoLayout
deriving anyclass instance IsoValue MarketplaceTezStorage
deriving anyclass instance HasAnnotation MarketplaceTezStorage

initMarketplaceTezStorage :: AdminStorage -> MarketplaceTezStorage
initMarketplaceTezStorage as = MarketplaceTezStorage as mempty

data MarketplaceTezEntrypoints
  = Sell InitSaleParamTez
  | Buy SaleParamTez
  | Cancel SaleParamTez
  | Admin AdminEntrypoints

customGeneric "MarketplaceTezEntrypoints" ligoLayout
deriving anyclass instance IsoValue MarketplaceTezEntrypoints
deriving anyclass instance HasAnnotation MarketplaceTezEntrypoints

instance ParameterHasEntrypoints MarketplaceTezEntrypoints where
  type ParameterEntrypointsDerivation MarketplaceTezEntrypoints = EpdRecursive

-- Contract
----------------------------------------------------------------------------

marketplaceTezContract :: T.Contract (ToT MarketplaceTezEntrypoints) (ToT MarketplaceTezStorage)
marketplaceTezContract = $$(embedContractM (inBinFolder "fixed_price_sale_market_tez.tz"))

-- Errors
----------------------------------------------------------------------------
