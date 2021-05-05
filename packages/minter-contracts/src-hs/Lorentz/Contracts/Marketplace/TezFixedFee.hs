-- | Lorentz bindings for the fixed price sale contract (FA2 version).
module Lorentz.Contracts.Marketplace.TezFixedFee
  ( -- * Types
    MarketFA2FixedFee.FeeData(..)
  , MarketTez.SaleId(..)
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
import Michelson.Test.Import (embedContractM)
import qualified Michelson.Typed as T

import qualified Lorentz.Contracts.Marketplace.FA2FixedFee as MarketFA2FixedFee
import qualified Lorentz.Contracts.Marketplace.Tez as MarketTez

-- Types
----------------------------------------------------------------------------

data MarketplaceTezStorage = MarketplaceTezStorage
  { admin :: AdminStorage
  , sales :: BigMap MarketTez.SaleId MarketTez.SaleParamTez
  , nextSaleId :: MarketTez.SaleId
  , fee :: MarketFA2FixedFee.FeeData
  }

customGeneric "MarketplaceTezStorage" ligoCombLayout
deriving anyclass instance IsoValue MarketplaceTezStorage
deriving anyclass instance HasAnnotation MarketplaceTezStorage
instance Buildable MarketplaceTezStorage where build = genericF

initMarketplaceTezStorage :: MarketFA2FixedFee.FeeData -> AdminStorage -> MarketplaceTezStorage
initMarketplaceTezStorage feeData as =
  MarketplaceTezStorage
    { admin = as
    , sales = mempty
    , nextSaleId = MarketTez.SaleId 0
    , fee = feeData
    }

-- Contract
----------------------------------------------------------------------------

marketplaceTezFixedFeeContract :: T.Contract (ToT MarketTez.MarketplaceTezEntrypoints) (ToT MarketplaceTezStorage)
marketplaceTezFixedFeeContract = $$(embedContractM (inBinFolder "fixed_price_sale_tez_fixed_fee.tz"))
