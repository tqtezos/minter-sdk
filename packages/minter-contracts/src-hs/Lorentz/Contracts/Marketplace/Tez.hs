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

data SaleId = SaleId Natural
  deriving stock (Eq, Ord)

customGeneric "SaleId" ligoCombLayout
deriving anyclass instance IsoValue SaleId
deriving anyclass instance HasAnnotation SaleId

data SaleToken = SaleToken
  { fa2Address :: Address
  , tokenId :: TokenId
  } deriving stock (Eq, Ord)

customGeneric "SaleToken" ligoCombLayout
deriving anyclass instance IsoValue SaleToken
deriving anyclass instance HasAnnotation SaleToken

data SaleDataTez = SaleDataTez 
  { saleToken :: SaleToken 
  , salePricePerToken :: Mutez 
  , tokenAmount :: Natural
  } deriving stock (Eq, Ord)

customGeneric "SaleDataTez" ligoCombLayout
deriving anyclass instance IsoValue SaleDataTez
deriving anyclass instance HasAnnotation SaleDataTez

data SaleParamTez = SaleParamTez
  { seller :: Address
  , saleDataTez :: SaleDataTez
  } deriving stock (Eq, Ord)

customGeneric "SaleParamTez" ligoCombLayout
deriving anyclass instance IsoValue SaleParamTez
deriving anyclass instance HasAnnotation SaleParamTez

data MarketplaceTezStorage = MarketplaceTezStorage
  { admin :: AdminStorage
  , nextSaleId :: SaleId
  , sales :: BigMap SaleId SaleParamTez
  }

customGeneric "MarketplaceTezStorage" ligoCombLayout
deriving anyclass instance IsoValue MarketplaceTezStorage
deriving anyclass instance HasAnnotation MarketplaceTezStorage

initMarketplaceTezStorage :: AdminStorage -> MarketplaceTezStorage
initMarketplaceTezStorage as = MarketplaceTezStorage as (SaleId 0) mempty

data MarketplaceTezEntrypoints
  = Sell SaleDataTez
  | Buy SaleId
  | Cancel SaleId
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
