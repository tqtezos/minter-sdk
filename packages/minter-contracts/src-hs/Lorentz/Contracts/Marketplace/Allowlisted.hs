-- | Lorentz bindings for the fixed price sale contract
-- with allowlist.
module Lorentz.Contracts.Marketplace.Allowlisted where

import Lorentz

import Lorentz.Contracts.Marketplace.FA2
import Lorentz.Contracts.Marketplace.Tez
import Lorentz.Contracts.MinterSdk
import Michelson.Test.Import (embedContractM)
import qualified Michelson.Typed as T

-- Types
----------------------------------------------------------------------------

data AllowlistedStorage marketStorage = AllowlistedStorage
  { marketStorage :: marketStorage
  , allowlist :: BigMap Address ()
  }

customGeneric "AllowlistedStorage" ligoLayout
deriving anyclass instance
  IsoValue s =>
  IsoValue (AllowlistedStorage s)
deriving anyclass instance
  HasAnnotation s =>
  HasAnnotation (AllowlistedStorage s)

initAllowlistedStorage :: marketStorage -> AllowlistedStorage marketStorage
initAllowlistedStorage ms = AllowlistedStorage ms mempty

data AllowlistedEntrypoints marketEntrypoints
  = Call_market marketEntrypoints
  | Update_allowed (BigMap Address ())

customGeneric "AllowlistedEntrypoints" ligoCombLayout
deriving anyclass instance
  IsoValue ep =>
  IsoValue (AllowlistedEntrypoints ep)
deriving anyclass instance
  HasAnnotation ep =>
  HasAnnotation (AllowlistedEntrypoints ep)

instance ParameterHasEntrypoints (AllowlistedEntrypoints MarketplaceEntrypoints) where
  type ParameterEntrypointsDerivation (AllowlistedEntrypoints MarketplaceEntrypoints) = EpdRecursive

instance ParameterHasEntrypoints (AllowlistedEntrypoints MarketplaceTezEntrypoints) where
  type ParameterEntrypointsDerivation (AllowlistedEntrypoints MarketplaceTezEntrypoints) = EpdRecursive

-- Contract
----------------------------------------------------------------------------

marketplaceAllowlistedContract
  :: T.Contract
      (ToT (AllowlistedEntrypoints MarketplaceEntrypoints))
      (ToT (AllowlistedStorage MarketplaceStorage))
marketplaceAllowlistedContract =
  $$(embedContractM (inBinFolder "fixed_price_sale_market_allowlisted.tz"))

marketplaceTezAllowlistedContract
  :: T.Contract
      (ToT (AllowlistedEntrypoints MarketplaceTezEntrypoints))
      (ToT (AllowlistedStorage MarketplaceTezStorage))
marketplaceTezAllowlistedContract =
  $$(embedContractM (inBinFolder "fixed_price_sale_market_tez_allowlisted.tz"))

-- Errors
----------------------------------------------------------------------------

saleAddressNotAllowed :: MText
saleAddressNotAllowed = [mt|SALE_ADDRESS_NOT_ALLOWED|]
