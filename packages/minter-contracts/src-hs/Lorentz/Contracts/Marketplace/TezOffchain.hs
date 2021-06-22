-- | Lorentz bindings for the fixed price sale contract (FA2 version).
module Lorentz.Contracts.Marketplace.TezOffchain
  ( -- * Types
    MarketTez.SaleId(..)
  , MarketTez.SaleToken(..)
  , MarketTez.SaleDataTez(..)
  , MarketTez.SaleParamTez(..)
  , OffchainBuyParam(..)
  , PendingPurchase(..)
  , Permit(..)
  , MarketplaceTezOffchainEntrypoints
  , MarketplaceTezOffchainStorage(..)

  -- * Contract
  , marketplaceTezOffchainContract
  , initMarketplaceTezOffchainStorage
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
data Permit = Permit
  { signerKey :: PublicKey
  , signature :: Signature
  } deriving stock (Eq, Ord)

customGeneric "Permit" ligoCombLayout
deriving anyclass instance IsoValue Permit
deriving anyclass instance HasAnnotation Permit
instance Buildable Permit where build = genericF

data MarketplaceTezOffchainStorage al = MarketplaceTezOffchainStorage
  { marketplaceStorage :: MarketTez.MarketplaceTezStorageWithPendingPurchases al
  , counter :: Natural
  }

customGeneric "MarketplaceTezOffchainStorage" ligoCombLayout
deriving anyclass instance IsoValue al => IsoValue (MarketplaceTezOffchainStorage al)
deriving anyclass instance HasAnnotation al => HasAnnotation (MarketplaceTezOffchainStorage al)
instance Buildable al => Buildable (MarketplaceTezOffchainStorage al) where build = genericF

data PendingPurchase = PendingPurchase
  { saleId :: MarketTez.SaleId
  , purchaser :: Address
  } deriving stock (Eq, Ord)

customGeneric "PendingPurchase" ligoCombLayout
deriving anyclass instance IsoValue PendingPurchase
deriving anyclass instance HasAnnotation PendingPurchase
instance Buildable PendingPurchase where build = genericF

data OffchainBuyParam = OffchainBuyParam
  { saleId :: MarketTez.SaleId
  , permit :: Permit
  } deriving stock (Eq, Ord)

customGeneric "OffchainBuyParam" ligoCombLayout
deriving anyclass instance IsoValue OffchainBuyParam
deriving anyclass instance HasAnnotation OffchainBuyParam
instance Buildable OffchainBuyParam where build = genericF

data MarketplaceTezOffchainEntrypoints al 
  = BaseSale (MarketTez.MarketplaceTezEntrypoints al)
  | Offchain_buy [OffchainBuyParam]
  | Confirm_purchases [PendingPurchase]
  | Revoke_purchases [PendingPurchase]

customGeneric "MarketplaceTezOffchainEntrypoints" ligoLayout
deriving anyclass instance IsoValue al => IsoValue (MarketplaceTezOffchainEntrypoints al)
deriving anyclass instance HasAnnotation al => HasAnnotation (MarketplaceTezOffchainEntrypoints al)

instance
  ( RequireAllUniqueEntrypoints (MarketplaceTezOffchainEntrypoints al), IsoValue al
  , EntrypointsDerivation EpdDelegate (MarketplaceTezOffchainEntrypoints al)
  ) =>
    ParameterHasEntrypoints (MarketplaceTezOffchainEntrypoints al) where
  type ParameterEntrypointsDerivation (MarketplaceTezOffchainEntrypoints al) = EpdDelegate

initMarketplaceTezOffchainStorage :: Monoid al => AdminStorage -> MarketplaceTezOffchainStorage al
initMarketplaceTezOffchainStorage as =
  MarketplaceTezOffchainStorage
    { marketplaceStorage = MarketTez.initMarketplaceStorageWithPendingPurchasers as 
    , counter = 0
    }

-- Contract
----------------------------------------------------------------------------

marketplaceTezOffchainContract
  :: T.Contract
      (ToT (MarketplaceTezOffchainEntrypoints NoAllowlist.Entrypoints))
      (ToT (MarketplaceTezOffchainStorage NoAllowlist.Allowlist))
marketplaceTezOffchainContract = $$(embedContractM (inBinFolder "fixed_price_sale_market_tez_offchain.tz"))
