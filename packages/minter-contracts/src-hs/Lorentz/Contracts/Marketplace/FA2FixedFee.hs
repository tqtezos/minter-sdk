-- | Lorentz bindings for the fixed price sale contract (FA2 version).
module Lorentz.Contracts.Marketplace.FA2FixedFee
  ( -- * Types
    FeeData(..)
  , MarketFA2.SaleId(..)
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
import Michelson.Test.Import (embedContractM)
import qualified Michelson.Typed as T

import qualified Lorentz.Contracts.Marketplace.FA2 as MarketFA2

-- Types
----------------------------------------------------------------------------

data FeeData = FeeData
  { feeAddress :: Address
  , feePercent :: Natural
  }

customGeneric "FeeData" rightComb
deriving anyclass instance IsoValue FeeData
deriving anyclass instance HasAnnotation FeeData
instance Buildable FeeData where build = genericF

data MarketplaceStorage = MarketplaceStorage
  { admin :: AdminStorage
  , sales :: BigMap MarketFA2.SaleId MarketFA2.SaleParam
  , nextSaleId :: MarketFA2.SaleId
  , fee :: FeeData
  }

customGeneric "MarketplaceStorage" ligoCombLayout
deriving anyclass instance IsoValue MarketplaceStorage
deriving anyclass instance HasAnnotation MarketplaceStorage
instance Buildable MarketplaceStorage where build = genericF

initMarketplaceStorage :: FeeData -> AdminStorage -> MarketplaceStorage
initMarketplaceStorage feeData as =
  MarketplaceStorage
    { admin = as
    , sales = mempty
    , nextSaleId = MarketFA2.SaleId 0
    , fee = feeData
    }

-- Contract
----------------------------------------------------------------------------

marketplaceFixedFeeContract :: T.Contract (ToT MarketFA2.MarketplaceEntrypoints) (ToT MarketplaceStorage)
marketplaceFixedFeeContract = $$(embedContractM (inBinFolder "fixed_price_sale_market_fixed_fee.tz"))
