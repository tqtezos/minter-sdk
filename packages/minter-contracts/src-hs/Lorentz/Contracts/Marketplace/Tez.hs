-- | Lorentz bindings for the fixed price sale contract (Tez version).
module Lorentz.Contracts.Marketplace.Tez where

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

data SaleParamTezPermit = SaleParamTezPermit
  { seller :: Address
  , saleDataTez :: SaleDataTez
  , pendingPurchases :: Set Address
  } deriving stock (Eq, Ord)

customGeneric "SaleParamTezPermit" ligoCombLayout
deriving anyclass instance IsoValue SaleParamTezPermit
deriving anyclass instance HasAnnotation SaleParamTezPermit
instance Buildable SaleParamTezPermit where build = genericF

data MarketplaceTezStorage al = MarketplaceTezStorage
  { sales :: BigMap SaleId SaleParamTez
  , admin :: AdminStorage
  , nextSaleId :: SaleId
  , allowlist :: al
  }

customGeneric "MarketplaceTezStorage" ligoCombLayout
deriving anyclass instance IsoValue al => IsoValue (MarketplaceTezStorage al)
deriving anyclass instance HasAnnotation al => HasAnnotation (MarketplaceTezStorage al)

data MarketplaceTezStorageWithPendingPurchases al = MarketplaceTezStorageWithPendingPurchases 
  { sales :: BigMap SaleId SaleParamTezPermit
  , admin :: AdminStorage
  , nextSaleId :: SaleId
  , allowlist :: al
  }

customGeneric "MarketplaceTezStorageWithPendingPurchases" ligoCombLayout
deriving anyclass instance IsoValue al => IsoValue (MarketplaceTezStorageWithPendingPurchases al)
deriving anyclass instance HasAnnotation al => HasAnnotation (MarketplaceTezStorageWithPendingPurchases al)

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

-- Contract
----------------------------------------------------------------------------

marketplaceTezContract
  :: T.Contract
      (ToT (MarketplaceTezEntrypoints NoAllowlist.Entrypoints))
      (ToT (MarketplaceTezStorage NoAllowlist.Allowlist))
marketplaceTezContract =
  $$(embedContractM (inBinFolder "fixed_price_sale_market_tez.tz"))

marketplaceTezAllowlistedContract
  :: T.Contract
      (ToT (MarketplaceTezEntrypoints AllowlistSimple.Entrypoints))
      (ToT (MarketplaceTezStorage AllowlistSimple.Allowlist))
marketplaceTezAllowlistedContract =
  $$(embedContractM (inBinFolder "fixed_price_sale_market_tez_allowlisted.tz"))

marketplaceTezAllowlistedTokenContract
  :: T.Contract
      (ToT (MarketplaceTezEntrypoints AllowlistToken.Entrypoints))
      (ToT (MarketplaceTezStorage AllowlistToken.Allowlist))
marketplaceTezAllowlistedTokenContract =
  $$(embedContractM (inBinFolder "fixed_price_sale_market_tez_allowlisted_token.tz"))

-- Errors
----------------------------------------------------------------------------
