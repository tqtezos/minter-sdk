-- | Lorentz bindings for the fixed price sale contract (Tez version).
module Lorentz.Contracts.Marketplace.Tez where

import Fmt (Buildable(..), genericF)
import Lorentz

import Lorentz.Contracts.MinterSdk
import Lorentz.Contracts.PausableAdminOption
import Lorentz.Contracts.Spec.FA2Interface
import Michelson.Test.Import (embedContractM)
import qualified Michelson.Typed as T

-- Types
----------------------------------------------------------------------------

newtype SaleId = SaleId Natural
  deriving stock (Generic, Eq, Ord)
  deriving newtype (IsoValue, HasAnnotation, Buildable)

data SaleToken = SaleToken
  { fa2Address :: Address
  , tokenId :: TokenId
  } deriving stock (Eq, Ord)

customGeneric "SaleToken" ligoCombLayout
deriving anyclass instance IsoValue SaleToken
deriving anyclass instance HasAnnotation SaleToken
instance Buildable SaleToken where build = genericF

data SaleDataTez = SaleDataTez
  { saleToken :: SaleToken
  , salePricePerToken :: Mutez
  , tokenAmount :: Natural
  } deriving stock (Eq, Ord)

customGeneric "SaleDataTez" ligoCombLayout
deriving anyclass instance IsoValue SaleDataTez
deriving anyclass instance HasAnnotation SaleDataTez
instance Buildable SaleDataTez where build = genericF

data SaleParamTez = SaleParamTez
  { seller :: Address
  , saleDataTez :: SaleDataTez
  } deriving stock (Eq, Ord)

customGeneric "SaleParamTez" ligoCombLayout
deriving anyclass instance IsoValue SaleParamTez
deriving anyclass instance HasAnnotation SaleParamTez
instance Buildable SaleParamTez where build = genericF

data MarketplaceTezStorage = MarketplaceTezStorage
  { admin :: AdminStorage
  , sales :: BigMap SaleId SaleParamTez
  , nextSaleId :: SaleId
  }

customGeneric "MarketplaceTezStorage" ligoCombLayout
deriving anyclass instance IsoValue MarketplaceTezStorage
deriving anyclass instance HasAnnotation MarketplaceTezStorage

initMarketplaceTezStorage :: AdminStorage -> MarketplaceTezStorage
initMarketplaceTezStorage as = MarketplaceTezStorage as mempty (SaleId 0)

data ManageSaleEntrypoints = Cancel SaleId
  | Admin AdminEntrypoints
  | Sell SaleDataTez

customGeneric "ManageSaleEntrypoints" ligoLayout
deriving anyclass instance IsoValue ManageSaleEntrypoints
deriving anyclass instance HasAnnotation ManageSaleEntrypoints

data MarketplaceTezEntrypoints
  = Buy SaleId
  | ManageSale ManageSaleEntrypoints 

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
