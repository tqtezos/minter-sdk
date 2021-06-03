-- | Lorentz bindings for the fixed price sale contract (FA2 version).
module Lorentz.Contracts.Marketplace.TezPermit
  ( -- * Types
    MarketTez.SaleId(..)
  , MarketTez.SaleToken(..)
  , MarketTez.SaleDataTez(..)
  , MarketTez.SaleParamTez(..)
  , MarketplaceTezPermitEntrypoints
  , MarketplaceTezPermitStorage
  , initMarketplaceTezPermitStorage

  -- * Contract
  , marketplaceTezPermitContract
  ) where

import Fmt (Buildable(..), genericF)
import Lorentz

import Lorentz.Contracts.MinterSdk
import Lorentz.Contracts.PausableAdminOption
import Michelson.Test.Import (embedContractM)
import qualified Michelson.Typed as T

import qualified Lorentz.Contracts.Marketplace.Tez as MarketTez
import qualified Lorentz.Contracts.NoAllowlist as NoAllowlist

-- Types
----------------------------------------------------------------------------

data MarketplaceTezPermitStorage al = MarketplaceTezPermitStorage
  { marketplaceStorage :: (MarketTez.MarketplaceTezStorageWithPendingPurchases al)
  , counter :: Natural
  }

customGeneric "MarketplaceTezPermitStorage" ligoCombLayout
deriving anyclass instance IsoValue al => IsoValue (MarketplaceTezPermitStorage al)
deriving anyclass instance HasAnnotation al => HasAnnotation (MarketplaceTezPermitStorage al)


data PendingPurchase = PendingPurchase
  { saleId :: MarketTez.SaleId
  , buyData :: MarketTez.BuyData
  } deriving stock (Eq, Ord)

customGeneric "PendingPurchase" ligoCombLayout
deriving anyclass instance IsoValue PendingPurchase
deriving anyclass instance HasAnnotation PendingPurchase
instance Buildable PendingPurchase where build = genericF

data Permit = Permit
  { signerKey :: PublicKey
  , signature :: Signature
  } deriving stock (Eq, Ord)

customGeneric "Permit" ligoCombLayout
deriving anyclass instance IsoValue Permit
deriving anyclass instance HasAnnotation Permit
instance Buildable Permit where build = genericF

data PermitBuyParam = PermitBuyParam
  { saleId :: MarketTez.SaleId
  , optionalPermit :: Maybe Permit
  } deriving stock (Eq, Ord)

customGeneric "PermitBuyParam" ligoCombLayout
deriving anyclass instance IsoValue PermitBuyParam
deriving anyclass instance HasAnnotation PermitBuyParam
instance Buildable PermitBuyParam where build = genericF

data MarketplaceTezPermitEntrypoints al 
  = BaseSale (MarketTez.ManageSaleTezEntrypoints al)
  | Permit_buy [PermitBuyParam]
  | Confirm_purchases [PendingPurchase]
  | Revoke_purchases [PendingPurchase]

customGeneric "MarketplaceTezPermitEntrypoints" ligoLayout
deriving anyclass instance IsoValue al => IsoValue (MarketplaceTezPermitEntrypoints al)
deriving anyclass instance HasAnnotation al => HasAnnotation (MarketplaceTezPermitEntrypoints al)

instance
  ( RequireAllUniqueEntrypoints (MarketplaceTezPermitEntrypoints al), IsoValue al
  , EntrypointsDerivation EpdDelegate (MarketplaceTezPermitEntrypoints al)
  ) =>
    ParameterHasEntrypoints (MarketplaceTezPermitEntrypoints al) where
  type ParameterEntrypointsDerivation (MarketplaceTezPermitEntrypoints al) = EpdDelegate

initMarketplaceTezPermitStorage :: Monoid al => AdminStorage -> MarketplaceTezPermitStorage al
initMarketplaceTezPermitStorage as =
  MarketplaceTezPermitStorage
    { marketplaceStorage = MarketTez.initMarketplaceStorageWithPendingPurchasers as 
    , counter = 0
    }

-- Contract
----------------------------------------------------------------------------

marketplaceTezPermitContract
  :: T.Contract
      (ToT (MarketplaceTezPermitEntrypoints NoAllowlist.Entrypoints))
      (ToT (MarketplaceTezPermitStorage NoAllowlist.Allowlist))
marketplaceTezPermitContract = $$(embedContractM (inBinFolder "fixed_price_sale_market_tez_offchain.tz"))
