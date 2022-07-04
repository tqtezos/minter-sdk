-- | Lorentz bindings for the fixed price sale contract (Tez version).
module Lorentz.Contracts.Marketplace.Tez where

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

data SaleParamTezOffchain = SaleParamTezOffchain
  { seller :: Address
  , saleDataTez :: SaleDataTez
  , pendingPurchases :: Set Address
  } deriving stock (Eq, Ord)

customGeneric "SaleParamTezOffchain" ligoCombLayout
deriving anyclass instance IsoValue SaleParamTezOffchain
deriving anyclass instance HasAnnotation SaleParamTezOffchain
instance Buildable SaleParamTezOffchain where build = genericF

data MarketplaceTezStorage al = MarketplaceTezStorage
  { sales :: BigMap SaleId SaleParamTez
  , admin :: AdminStorage
  , nextSaleId :: SaleId
  , allowlist :: al
  } deriving stock (Eq)

customGeneric "MarketplaceTezStorage" ligoCombLayout
deriving anyclass instance IsoValue al => IsoValue (MarketplaceTezStorage al)
deriving anyclass instance HasAnnotation al => HasAnnotation (MarketplaceTezStorage al)

data MarketplaceTezStorageWithPendingPurchases al = MarketplaceTezStorageWithPendingPurchases 
  { sales :: BigMap SaleId SaleParamTezOffchain
  , admin :: AdminStorage
  , nextSaleId :: SaleId
  , allowlist :: al
  } deriving stock (Eq)

customGeneric "MarketplaceTezStorageWithPendingPurchases" ligoCombLayout
deriving anyclass instance IsoValue al => IsoValue (MarketplaceTezStorageWithPendingPurchases al)
deriving anyclass instance HasAnnotation al => HasAnnotation (MarketplaceTezStorageWithPendingPurchases al)
instance Buildable al => Buildable (MarketplaceTezStorageWithPendingPurchases al) where build = genericF

initMarketplaceTezStorage :: Monoid al => AdminStorage -> MarketplaceTezStorage al
initMarketplaceTezStorage as = MarketplaceTezStorage mempty as (SaleId 0) mempty

initMarketplaceStorageWithPendingPurchasers :: Monoid al => AdminStorage -> MarketplaceTezStorageWithPendingPurchases al
initMarketplaceStorageWithPendingPurchasers as = MarketplaceTezStorageWithPendingPurchases mempty as (SaleId 0) mempty

data MarketplaceTezEntrypoints al
  = Sell SaleDataTez
  | Buy SaleId
  | Cancel SaleId
  | Admin AdminEntrypoints
  | Update_allowed al

customGeneric "MarketplaceTezEntrypoints" ligoLayout
deriving anyclass instance IsoValue al => IsoValue (MarketplaceTezEntrypoints al)
deriving anyclass instance HasAnnotation al => HasAnnotation (MarketplaceTezEntrypoints al)

instance
  ( RequireAllUniqueEntrypoints (MarketplaceTezEntrypoints al), IsoValue al
  , EntrypointsDerivation EpdDelegate (MarketplaceTezEntrypoints al)
  ) =>
    ParameterHasEntrypoints (MarketplaceTezEntrypoints al) where
  type ParameterEntrypointsDerivation (MarketplaceTezEntrypoints al) = EpdDelegate

-- Errors
----------------------------------------------------------------------------
