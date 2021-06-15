-- | Lorentz bindings for the fixed price sale contract (FA2 version).
module Lorentz.Contracts.Marketplace.TezFixedFee
  ( -- * Types
    MarketTez.SaleId(..)
  , MarketTez.SaleToken(..)
  , MarketTez.SaleDataTez(..)
  , MarketTez.SaleParamTez(..)
  , MarketplaceTezStorage(..)
  , MarketTez.MarketplaceTezEntrypoints(..)
  , initMarketplaceTezStorage

  -- * Contract
  , marketplaceTezFixedFeeContract
  ) where

import Fmt (Buildable(..), genericF)
import Lorentz

import Lorentz.Contracts.MinterSdk
import Lorentz.Contracts.PausableAdminOption
import Lorentz.Test.Import (embedContractM)
import qualified Michelson.Typed as T

import qualified Lorentz.Contracts.Marketplace.Tez as MarketTez
import qualified Lorentz.Contracts.NoAllowlist as NoAllowlist

-- Types
----------------------------------------------------------------------------

data MarketplaceTezStorage al = MarketplaceTezStorage
  { sales :: BigMap MarketTez.SaleId MarketTez.SaleParamTez
  , admin :: AdminStorage
  , nextSaleId :: MarketTez.SaleId
  , allowlist :: al
  , fee :: FeeData
  }

customGeneric "MarketplaceTezStorage" ligoCombLayout
deriving anyclass instance IsoValue al => IsoValue (MarketplaceTezStorage al)
deriving anyclass instance HasAnnotation al => HasAnnotation (MarketplaceTezStorage al)
instance Buildable al => Buildable (MarketplaceTezStorage al) where build = genericF

initMarketplaceTezStorage :: Monoid al => FeeData -> AdminStorage -> MarketplaceTezStorage al
initMarketplaceTezStorage feeData as =
  MarketplaceTezStorage
    { admin = as
    , sales = mempty
    , nextSaleId = MarketTez.SaleId 0
    , allowlist = mempty
    , fee = feeData
    }

-- Contract
----------------------------------------------------------------------------

marketplaceTezFixedFeeContract
  :: Contract
      (MarketTez.MarketplaceTezEntrypoints NoAllowlist.Entrypoints)
      (MarketplaceTezStorage NoAllowlist.Allowlist)
marketplaceTezFixedFeeContract =
  $$(embedContractM (inBinFolder "fixed_price_sale_tez_fixed_fee.tz"))
