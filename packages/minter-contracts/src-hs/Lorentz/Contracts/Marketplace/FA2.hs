-- | Lorentz bindings for the fixed price sale contract (FA2 version).
module Lorentz.Contracts.Marketplace.FA2 where

import Fmt (Buildable(..), genericF)
import Lorentz

import Lorentz.Contracts.PausableAdminOption
import Lorentz.Contracts.Spec.FA2Interface

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

data SaleParamPermit = SaleParamPermit
  { seller :: Address
  , saleData :: SaleData
  , pendingPurchases :: Set Address
  } deriving stock (Eq, Ord)

customGeneric "SaleParamPermit" ligoCombLayout
deriving anyclass instance IsoValue SaleParamPermit
deriving anyclass instance HasAnnotation SaleParamPermit
instance Buildable SaleParamPermit where build = genericF

data MarketplaceStorage al = MarketplaceStorage
  { sales :: BigMap SaleId SaleParam
  , admin :: AdminStorage
  , nextSaleId :: SaleId
  , allowlist :: al
  }

customGeneric "MarketplaceStorage" ligoCombLayout
deriving anyclass instance IsoValue al => IsoValue (MarketplaceStorage al)
deriving anyclass instance HasAnnotation al => HasAnnotation (MarketplaceStorage al)

data MarketplaceStorageWithPendingPurchases al = MarketplaceStorageWithPendingPurchases
  { sales :: BigMap SaleId SaleParamPermit
  , admin :: AdminStorage
  , nextSaleId :: SaleId
  , allowlist :: al
  }

customGeneric "MarketplaceStorageWithPendingPurchases" ligoCombLayout
deriving anyclass instance IsoValue al => IsoValue (MarketplaceStorageWithPendingPurchases al)
deriving anyclass instance HasAnnotation al => HasAnnotation (MarketplaceStorageWithPendingPurchases al)

initMarketplaceStorage :: Monoid al => AdminStorage -> MarketplaceStorage al
initMarketplaceStorage as = MarketplaceStorage mempty as (SaleId 0) mempty

initMarketplaceStorageWithPendingPurchases :: Monoid al => AdminStorage -> MarketplaceStorageWithPendingPurchases  al
initMarketplaceStorageWithPendingPurchases as = MarketplaceStorageWithPendingPurchases mempty as (SaleId 0) mempty

data MarketplaceEntrypoints al
  = Sell SaleData
  | Buy SaleId
  | Cancel SaleId
  | Admin AdminEntrypoints
  | Update_allowed al

customGeneric "MarketplaceEntrypoints" ligoLayout
deriving anyclass instance IsoValue al => IsoValue (MarketplaceEntrypoints al)
deriving anyclass instance HasAnnotation al => HasAnnotation (MarketplaceEntrypoints al)

instance
  ( RequireAllUniqueEntrypoints (MarketplaceEntrypoints al), IsoValue al
  , EntrypointsDerivation EpdDelegate (MarketplaceEntrypoints al)
  ) =>
    ParameterHasEntrypoints (MarketplaceEntrypoints al) where
  type ParameterEntrypointsDerivation (MarketplaceEntrypoints al) = EpdDelegate
