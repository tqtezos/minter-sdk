-- | Lorentz bindings for the fixed price sale contract (FA2 version).
module Lorentz.Contracts.Marketplace.FA2Permit
  ( -- * Types
    MarketFA2.SaleId(..)
  , MarketFA2.SaleToken(..)
  , MarketFA2.SaleData(..)
  , MarketFA2.SaleParam(..)
  , initMarketplaceFA2PermitStorage

  -- * Contract
  , marketplaceFA2PermitContract
  ) where

import Fmt (Buildable(..), genericF)
import Lorentz

import Lorentz.Contracts.MinterSdk
import Lorentz.Contracts.PausableAdminOption
import Michelson.Test.Import (embedContractM)
import qualified Michelson.Typed as T

import qualified Lorentz.Contracts.Marketplace.FA2 as MarketFA2
import qualified Lorentz.Contracts.NoAllowlist as NoAllowlist

-- Types
----------------------------------------------------------------------------

data MarketplaceFA2PermitStorage al = MarketplaceFA2PermitStorage
  { marketplaceStorage :: (MarketFA2.MarketplacePermitStorage al)
  , counter :: Natural
  }

customGeneric "MarketplaceFA2PermitStorage" ligoCombLayout
deriving anyclass instance IsoValue al => IsoValue (MarketplaceFA2PermitStorage al)
deriving anyclass instance HasAnnotation al => HasAnnotation (MarketplaceFA2PermitStorage al)


data PendingPurchase = PendingPurchase
  { saleId :: MarketFA2.SaleId
  , buyData :: MarketFA2.BuyData
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
  { saleId :: MarketFA2.SaleId
  , optionalPermit :: Maybe Permit
  } deriving stock (Eq, Ord)

customGeneric "PermitBuyParam" ligoCombLayout
deriving anyclass instance IsoValue PermitBuyParam
deriving anyclass instance HasAnnotation PermitBuyParam
instance Buildable PermitBuyParam where build = genericF

data MarketplaceFA2PermitEntrypoints al 
  = BaseSale (MarketFA2.ManageSaleEntrypoints al)
  | Permit_buy [PermitBuyParam]
  | Confirm_purchases [PendingPurchase]
  | Revoke_purchases [PendingPurchase]

customGeneric "MarketplaceFA2PermitEntrypoints" ligoLayout
deriving anyclass instance IsoValue al => IsoValue (MarketplaceFA2PermitEntrypoints al)
deriving anyclass instance HasAnnotation al => HasAnnotation (MarketplaceFA2PermitEntrypoints al)

instance
  ( RequireAllUniqueEntrypoints (MarketplaceFA2PermitEntrypoints al), IsoValue al
  , EntrypointsDerivation EpdDelegate (MarketplaceFA2PermitEntrypoints al)
  ) =>
    ParameterHasEntrypoints (MarketplaceFA2PermitEntrypoints al) where
  type ParameterEntrypointsDerivation (MarketplaceFA2PermitEntrypoints al) = EpdDelegate

initMarketplaceFA2PermitStorage :: Monoid al => AdminStorage -> MarketplaceFA2PermitStorage al
initMarketplaceFA2PermitStorage as =
  MarketplaceFA2PermitStorage
    { marketplaceStorage = MarketFA2.initMarketplacePermitStorage as 
    , counter = 0
    }

-- Contract
----------------------------------------------------------------------------

marketplaceFA2PermitContract
  :: T.Contract
      (ToT (MarketplaceFA2PermitEntrypoints NoAllowlist.Entrypoints))
      (ToT (MarketplaceFA2PermitStorage NoAllowlist.Allowlist))
marketplaceFA2PermitContract = $$(embedContractM (inBinFolder "fixed_price_sale_market_offchain.tz"))
