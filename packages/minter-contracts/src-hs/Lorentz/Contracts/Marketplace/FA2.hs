-- | Lorentz bindings for the fixed price sale contract (FA2 version).
module Lorentz.Contracts.Marketplace.FA2 where

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

data MoneyToken = MoneyToken
  { fa2Address :: Address
  , tokenId :: TokenId
  } deriving stock (Eq, Ord)

customGeneric "MoneyToken" ligoCombLayout
deriving anyclass instance IsoValue MoneyToken
deriving anyclass instance HasAnnotation MoneyToken
instance Buildable MoneyToken where build = genericF

data SaleData = SaleData
  { salePricePerToken :: Natural
  , saleToken :: SaleToken
  , moneyToken :: MoneyToken
  , tokenAmount :: Natural
  } deriving stock (Eq, Ord)

customGeneric "SaleData" ligoCombLayout
deriving anyclass instance IsoValue SaleData
deriving anyclass instance HasAnnotation SaleData
instance Buildable SaleData where build = genericF

data SaleParam = SaleParam
  { seller :: Address
  , saleData :: SaleData
  } deriving stock (Eq, Ord)

customGeneric "SaleParam" ligoCombLayout
deriving anyclass instance IsoValue SaleParam
deriving anyclass instance HasAnnotation SaleParam
instance Buildable SaleParam where build = genericF

data MarketplaceStorage = MarketplaceStorage
  { admin :: AdminStorage
  , sales :: BigMap SaleId SaleParam
  , nextSaleId :: SaleId
  }

customGeneric "MarketplaceStorage" ligoCombLayout
deriving anyclass instance IsoValue MarketplaceStorage
deriving anyclass instance HasAnnotation MarketplaceStorage

initMarketplaceStorage :: AdminStorage -> MarketplaceStorage
initMarketplaceStorage as = MarketplaceStorage as mempty (SaleId 0)

data ManageSaleEntrypoints = Cancel SaleId
  | Admin AdminEntrypoints
  | Sell SaleData

customGeneric "ManageSaleEntrypoints" ligoLayout
deriving anyclass instance IsoValue ManageSaleEntrypoints
deriving anyclass instance HasAnnotation ManageSaleEntrypoints

data MarketplaceEntrypoints
  = Buy SaleId
  | ManageSale ManageSaleEntrypoints 

customGeneric "MarketplaceEntrypoints" ligoLayout
deriving anyclass instance IsoValue MarketplaceEntrypoints
deriving anyclass instance HasAnnotation MarketplaceEntrypoints

instance ParameterHasEntrypoints MarketplaceEntrypoints where
  type ParameterEntrypointsDerivation MarketplaceEntrypoints = EpdRecursive

-- Contract
----------------------------------------------------------------------------

marketplaceContract :: T.Contract (ToT MarketplaceEntrypoints) (ToT MarketplaceStorage)
marketplaceContract = $$(embedContractM (inBinFolder "fixed_price_sale_market.tz"))

-- Errors
----------------------------------------------------------------------------
