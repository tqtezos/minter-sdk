-- | Lorentz marketplace contracts.
module Lorentz.Contracts.Marketplace.Contracts where

import Lorentz

import qualified Lorentz.Contracts.AllowlistSimple as AllowlistSimple
import qualified Lorentz.Contracts.AllowlistToken as AllowlistToken
import qualified Lorentz.Contracts.Marketplace.FA2 as MarketFA2
import qualified Lorentz.Contracts.Marketplace.FA2FixedFee as MarketFA2FixedFee
import qualified Lorentz.Contracts.Marketplace.Tez as MarketTez
import qualified Lorentz.Contracts.Marketplace.TezFixedFee as MarketTezFixedFee
import qualified Lorentz.Contracts.Marketplace.TezOffchain as TezOffchain
import Lorentz.Contracts.MinterSdk
import qualified Lorentz.Contracts.NoAllowlist as NoAllowlist
import Lorentz.Test.Import (embedContractM)

-- FA2
----------------------------------------------------------------------------

marketplaceContract
  :: Contract
      (MarketFA2.MarketplaceEntrypoints NoAllowlist.Entrypoints)
      (MarketFA2.MarketplaceStorage NoAllowlist.Allowlist)
marketplaceContract =
  $$(embedContractM (inBinFolder "fixed_price_sale_market.tz"))

marketplaceAllowlistedContract
  :: Contract
      (MarketFA2.MarketplaceEntrypoints AllowlistSimple.Entrypoints)
      (MarketFA2.MarketplaceStorage AllowlistSimple.Allowlist)
marketplaceAllowlistedContract =
  $$(embedContractM (inBinFolder "fixed_price_sale_market_allowlisted.tz"))

marketplaceAllowlistedTokenContract
  :: Contract
      (MarketFA2.MarketplaceEntrypoints AllowlistToken.Entrypoints)
      (MarketFA2.MarketplaceStorage AllowlistToken.Allowlist)
marketplaceAllowlistedTokenContract =
  $$(embedContractM (inBinFolder "fixed_price_sale_market_allowlisted_token.tz"))

-- FA2FixedFee
----------------------------------------------------------------------------

marketplaceFixedFeeContract
  :: Contract
      (MarketFA2.MarketplaceEntrypoints NoAllowlist.Entrypoints)
      (MarketFA2FixedFee.MarketplaceStorage NoAllowlist.Allowlist)
marketplaceFixedFeeContract =
  $$(embedContractM (inBinFolder "fixed_price_sale_market_fixed_fee.tz"))

-- Tez
----------------------------------------------------------------------------

marketplaceTezContract
  :: Contract
      (MarketTez.MarketplaceTezEntrypoints NoAllowlist.Entrypoints)
      (MarketTez.MarketplaceTezStorage NoAllowlist.Allowlist)
marketplaceTezContract =
  $$(embedContractM (inBinFolder "fixed_price_sale_market_tez.tz"))

marketplaceTezAllowlistedContract
  :: Contract
      (MarketTez.MarketplaceTezEntrypoints AllowlistSimple.Entrypoints)
      (MarketTez.MarketplaceTezStorage AllowlistSimple.Allowlist)
marketplaceTezAllowlistedContract =
  $$(embedContractM (inBinFolder "fixed_price_sale_market_tez_allowlisted.tz"))

marketplaceTezAllowlistedTokenContract
  :: Contract
      (MarketTez.MarketplaceTezEntrypoints AllowlistToken.Entrypoints)
      (MarketTez.MarketplaceTezStorage AllowlistToken.Allowlist)
marketplaceTezAllowlistedTokenContract =
  $$(embedContractM (inBinFolder "fixed_price_sale_market_tez_allowlisted_token.tz"))

-- TezFixedFee
----------------------------------------------------------------------------

marketplaceTezFixedFeeContract
  :: Contract
      (MarketTez.MarketplaceTezEntrypoints NoAllowlist.Entrypoints)
      (MarketTezFixedFee.MarketplaceTezStorage NoAllowlist.Allowlist)
marketplaceTezFixedFeeContract =
  $$(embedContractM (inBinFolder "fixed_price_sale_tez_fixed_fee.tz"))

-- TezOffchain
----------------------------------------------------------------------------

marketplaceTezOffchainContract
  :: Contract
      (TezOffchain.MarketplaceTezOffchainEntrypoints NoAllowlist.Entrypoints)
      (TezOffchain.MarketplaceTezOffchainStorage NoAllowlist.Allowlist)
marketplaceTezOffchainContract =
  $$(embedContractM (inBinFolder "fixed_price_sale_market_tez_offchain.tz"))
