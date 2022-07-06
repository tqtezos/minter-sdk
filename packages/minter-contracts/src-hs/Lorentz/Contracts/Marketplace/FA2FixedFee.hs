-- | Lorentz bindings for the fixed price sale contract (FA2 version).
module Lorentz.Contracts.Marketplace.FA2FixedFee
  ( -- * Types
    MarketFA2.SaleId(..)
  , MarketFA2.SaleToken(..)
  , MarketFA2.MoneyToken(..)
  , MarketFA2.SaleData(..)
  , MarketFA2.SaleParam(..)
  , MarketplaceStorage(..)
  , MarketFA2.MarketplaceEntrypoints(..)
  , initMarketplaceStorage
  ) where

import Fmt (Buildable(..), genericF)
import Lorentz

import Lorentz.Contracts.MinterSdk
import Lorentz.Contracts.PausableAdminOption

import qualified Lorentz.Contracts.Marketplace.FA2 as MarketFA2

-- Types
----------------------------------------------------------------------------

data MarketplaceStorage al = MarketplaceStorage
  { sales :: BigMap MarketFA2.SaleId MarketFA2.SaleParam
  , admin :: AdminStorage
  , nextSaleId :: MarketFA2.SaleId
  , allowlist :: al
  , fee :: FeeData
  }

customGeneric "MarketplaceStorage" ligoCombLayout
deriving anyclass instance IsoValue al => IsoValue (MarketplaceStorage al)
deriving anyclass instance HasAnnotation al => HasAnnotation (MarketplaceStorage al)
instance Buildable al => Buildable (MarketplaceStorage al) where build = genericF

initMarketplaceStorage :: Monoid al => FeeData -> AdminStorage -> MarketplaceStorage al
initMarketplaceStorage feeData as =
  MarketplaceStorage
    { admin = as
    , sales = mempty
    , nextSaleId = MarketFA2.SaleId 0
    , allowlist = mempty
    , fee = feeData
    }
