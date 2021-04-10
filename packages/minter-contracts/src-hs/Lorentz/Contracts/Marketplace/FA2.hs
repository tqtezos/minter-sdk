-- | Lorentz bindings for the fixed price sale contract (FA2 version).
module Lorentz.Contracts.Marketplace.FA2
  ( module Lorentz.Contracts.Marketplace.FA2
  ) where

import Lorentz

import Lorentz.Contracts.MinterSdk
import Lorentz.Contracts.PausableAdminOption
import Lorentz.Contracts.Spec.FA2Interface
import Michelson.Test.Import (embedContractM)
import qualified Michelson.Typed as T

-- Types
----------------------------------------------------------------------------

data SaleTokenParam = SaleTokenParam
  { tokenForSaleAddress :: Address
  , tokenForSaleTokenId :: TokenId
  , moneyTokenAddress :: Address
  , moneyTokenTokenId :: TokenId
  } deriving stock (Eq, Ord)

customGeneric "SaleTokenParam" ligoCombLayout
deriving anyclass instance IsoValue SaleTokenParam
deriving anyclass instance HasAnnotation SaleTokenParam

data SaleParam = SaleParam
  { saleSeller :: Address
  , tokens :: SaleTokenParam
  } deriving stock (Eq, Ord)

customGeneric "SaleParam" ligoCombLayout
deriving anyclass instance IsoValue SaleParam
deriving anyclass instance HasAnnotation SaleParam

data InitSaleParam = InitSaleParam
  { salePrice :: Natural
  , saleTokensParam :: SaleTokenParam
  }

customGeneric "InitSaleParam" ligoCombLayout
deriving anyclass instance IsoValue InitSaleParam
deriving anyclass instance HasAnnotation InitSaleParam

data MarketplaceStorage = MarketplaceStorage
  { admin :: AdminStorage
  , sales :: BigMap SaleParam Natural
  }

customGeneric "MarketplaceStorage" ligoLayout
deriving anyclass instance IsoValue MarketplaceStorage
deriving anyclass instance HasAnnotation MarketplaceStorage

initMarketplaceStorage :: AdminStorage -> MarketplaceStorage
initMarketplaceStorage as = MarketplaceStorage as mempty

data MarketplaceEntrypoints
  = Sell InitSaleParam
  | Buy SaleParam
  | Cancel SaleParam
  | Admin AdminEntrypoints

customGeneric "MarketplaceEntrypoints" ligoLayout
deriving anyclass instance IsoValue MarketplaceEntrypoints
deriving anyclass instance HasAnnotation MarketplaceEntrypoints

instance ParameterHasEntrypoints MarketplaceEntrypoints where
  type ParameterEntrypointsDerivation MarketplaceEntrypoints = EpdRecursive

-- This empty slice is a workaround, so that all the declarations above and
-- their instances may be in the type environment in the TH splice below.
$(pure [])

-- Contract
----------------------------------------------------------------------------

marketplaceContract :: T.Contract (ToT MarketplaceEntrypoints) (ToT MarketplaceStorage)
marketplaceContract = $$(embedContractM (inBinFolder "fixed_price_sale_market.tz"))

-- Errors
----------------------------------------------------------------------------
