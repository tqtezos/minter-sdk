-- | Lorentz bindings for the fixed price sale contract (FA2 version).
module Lorentz.Contracts.Marketplace.FA2 where

import Fmt (Buildable(..), genericF)
import Lorentz

import qualified Lorentz.Contracts.AllowlistSimple as AllowlistSimple
import qualified Lorentz.Contracts.AllowlistToken as AllowlistToken
import Lorentz.Contracts.MinterSdk
import qualified Lorentz.Contracts.NoAllowlist as NoAllowlist
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

data MarketplaceStorage al = MarketplaceStorage
  { sales :: BigMap SaleId SaleParam
  , admin :: AdminStorage
  , nextSaleId :: SaleId
  , allowlist :: al
  }

customGeneric "MarketplaceStorage" ligoCombLayout
deriving anyclass instance IsoValue al => IsoValue (MarketplaceStorage al)
deriving anyclass instance HasAnnotation al => HasAnnotation (MarketplaceStorage al)

initMarketplaceStorage :: Monoid al => AdminStorage -> MarketplaceStorage al
initMarketplaceStorage as = MarketplaceStorage mempty as (SaleId 0) mempty

data ManageSaleEntrypoints al = Cancel SaleId
  | Admin AdminEntrypoints
  | Sell SaleData
  | Update_allowed al

customGeneric "ManageSaleEntrypoints" ligoLayout
deriving anyclass instance IsoValue al => IsoValue (ManageSaleEntrypoints al)
deriving anyclass instance HasAnnotation al => HasAnnotation (ManageSaleEntrypoints al)

instance
  ( RequireAllUniqueEntrypoints (ManageSaleEntrypoints al), IsoValue al
  , EntrypointsDerivation EpdDelegate (ManageSaleEntrypoints al)
  ) =>
    ParameterHasEntrypoints (ManageSaleEntrypoints al) where
  type ParameterEntrypointsDerivation (ManageSaleEntrypoints al) = EpdDelegate

data MarketplaceEntrypoints al 
  = Buy SaleId
  | ManageSale (ManageSaleEntrypoints al)

customGeneric "MarketplaceEntrypoints" ligoLayout
deriving anyclass instance IsoValue al => IsoValue (MarketplaceEntrypoints al)
deriving anyclass instance HasAnnotation al => HasAnnotation (MarketplaceEntrypoints al)

instance
  ( RequireAllUniqueEntrypoints (MarketplaceEntrypoints al), IsoValue al
  , EntrypointsDerivation EpdDelegate (MarketplaceEntrypoints al)
  ) =>
    ParameterHasEntrypoints (MarketplaceEntrypoints al) where
  type ParameterEntrypointsDerivation (MarketplaceEntrypoints al) = EpdDelegate

-- Contract
----------------------------------------------------------------------------

marketplaceContract
  :: T.Contract
      (ToT (MarketplaceEntrypoints NoAllowlist.Entrypoints))
      (ToT (MarketplaceStorage NoAllowlist.Allowlist))
marketplaceContract =
  $$(embedContractM (inBinFolder "fixed_price_sale_market.tz"))

marketplaceAllowlistedContract
  :: T.Contract
      (ToT (MarketplaceEntrypoints AllowlistSimple.Entrypoints))
      (ToT (MarketplaceStorage AllowlistSimple.Allowlist))
marketplaceAllowlistedContract =
  $$(embedContractM (inBinFolder "fixed_price_sale_market_allowlisted.tz"))

marketplaceAllowlistedTokenContract
  :: T.Contract
      (ToT (MarketplaceEntrypoints AllowlistToken.Entrypoints))
      (ToT (MarketplaceStorage AllowlistToken.Allowlist))
marketplaceAllowlistedTokenContract =
  $$(embedContractM (inBinFolder "fixed_price_sale_market_allowlisted_token.tz"))

-- Errors
----------------------------------------------------------------------------
