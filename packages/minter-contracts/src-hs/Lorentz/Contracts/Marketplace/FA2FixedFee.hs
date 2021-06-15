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

  -- * Contract
  , marketplaceFixedFeeContract
  ) where

import Fmt (Buildable(..), genericF)
import Lorentz

import Lorentz.Contracts.MinterSdk
import Lorentz.Contracts.PausableAdminOption
import Lorentz.Test.Import (embedContractM)
import qualified Michelson.Typed as T

import qualified Lorentz.Contracts.Marketplace.FA2 as MarketFA2
import qualified Lorentz.Contracts.NoAllowlist as NoAllowlist

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

-- Contract
----------------------------------------------------------------------------

marketplaceFixedFeeContract
  :: Contract
      (MarketFA2.MarketplaceEntrypoints NoAllowlist.Entrypoints)
      (MarketplaceStorage NoAllowlist.Allowlist)
marketplaceFixedFeeContract =
  $$(embedContractM (inBinFolder "fixed_price_sale_market_fixed_fee.tz"))
