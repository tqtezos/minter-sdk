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
  , initMarketplaceTezOffchainStorage
  ) where

import Fmt (Buildable(..), genericF)
import Lorentz

import Lorentz.Contracts.PausableAdminOption

import qualified Lorentz.Contracts.Marketplace.Tez as MarketTez

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
  } deriving stock (Eq)

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
    , counter = 1
    }
