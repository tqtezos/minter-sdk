-- | English auction contracts.
module Lorentz.Contracts.EnglishAuction.Contracts where

import Lorentz

import qualified Lorentz.Contracts.AllowlistSimple as AllowlistSimple
import qualified Lorentz.Contracts.AllowlistToken as AllowlistToken
import qualified Lorentz.Contracts.EnglishAuction.FA2 as AuctionFA2
import qualified Lorentz.Contracts.EnglishAuction.FA2FixedFee as AuctionFA2FixedFee
import qualified Lorentz.Contracts.EnglishAuction.Tez as AuctionTez
import qualified Lorentz.Contracts.EnglishAuction.TezFixedFee as AuctionTezFixedFee
import qualified Lorentz.Contracts.EnglishAuction.TezPermit as AuctionTezPermit
import Lorentz.Contracts.MinterSdk
import qualified Lorentz.Contracts.NoAllowlist as NoAllowlist
import Lorentz.Test.Import (embedContractM)

-- AuctionFA2
----------------------------------------------------------------------------

englishAuctionFA2Contract
  :: Contract
      (AuctionFA2.AuctionEntrypoints NoAllowlist.Entrypoints)
      (AuctionFA2.AuctionStorage NoAllowlist.Allowlist)
englishAuctionFA2Contract =
  $$(embedContractM (inBinFolder "english_auction_fa2.tz"))

auctionFA2AllowlistedContract
  :: Contract
      (AuctionFA2.AuctionEntrypoints AllowlistSimple.Entrypoints)
      (AuctionFA2.AuctionStorage AllowlistSimple.Allowlist)
auctionFA2AllowlistedContract =
  $$(embedContractM (inBinFolder "english_auction_fa2_allowlisted.tz"))

auctionFA2AllowlistedTokenContract
  :: Contract
      (AuctionFA2.AuctionEntrypoints AllowlistToken.Entrypoints)
      (AuctionFA2.AuctionStorage AllowlistToken.Allowlist)
auctionFA2AllowlistedTokenContract =
  $$(embedContractM (inBinFolder "english_auction_fa2_allowlisted_token.tz"))

-- FA2FixedFee
----------------------------------------------------------------------------

englishAuctionFA2FixedFeeContract
  :: Contract
      (AuctionFA2.AuctionEntrypoints NoAllowlist.Entrypoints)
      AuctionFA2FixedFee.AuctionStorage
englishAuctionFA2FixedFeeContract =
  $$(embedContractM (inBinFolder "english_auction_fa2_fixed_fee.tz"))

-- Tez
----------------------------------------------------------------------------

auctionTezContract
  :: Contract
      (AuctionTez.AuctionEntrypoints NoAllowlist.Entrypoints)
      (AuctionTez.AuctionStorage NoAllowlist.Allowlist)
auctionTezContract =
  $$(embedContractM (inBinFolder "english_auction_tez.tz"))

auctionTezAllowlistedContract
  :: Contract
      (AuctionTez.AuctionEntrypoints AllowlistSimple.Entrypoints)
      (AuctionTez.AuctionStorage AllowlistSimple.Allowlist)
auctionTezAllowlistedContract =
  $$(embedContractM (inBinFolder "english_auction_tez_allowlisted.tz"))

auctionTezAllowlistedTokenContract
  :: Contract
      (AuctionTez.AuctionEntrypoints AllowlistToken.Entrypoints)
      (AuctionTez.AuctionStorage AllowlistToken.Allowlist)
auctionTezAllowlistedTokenContract =
  $$(embedContractM (inBinFolder "english_auction_tez_allowlisted_token.tz"))

-- TezFixedFee
----------------------------------------------------------------------------

englishAuctionTezFixedFeeContract
  :: Contract
      (AuctionTezFixedFee.AuctionEntrypoints NoAllowlist.Entrypoints)
      AuctionTezFixedFee.AuctionStorage
englishAuctionTezFixedFeeContract =
  $$(embedContractM (inBinFolder "english_auction_tez_fixed_fee.tz"))

-- TezPermit
----------------------------------------------------------------------------

auctionTezPermitContract
  :: Contract
      (AuctionTezPermit.PermitAuctionEntrypoints NoAllowlist.Entrypoints)
      (AuctionTezPermit.PermitAuctionStorage NoAllowlist.Allowlist)
auctionTezPermitContract =
  $$(embedContractM (inBinFolder "english_auction_tez_permit.tz"))

auctionTezPermitAllowlistedContract
  :: Contract
      (AuctionTezPermit.PermitAuctionEntrypoints AllowlistSimple.Entrypoints)
      (AuctionTezPermit.PermitAuctionStorage AllowlistSimple.Allowlist)
auctionTezPermitAllowlistedContract =
  $$(embedContractM (inBinFolder "english_auction_tez_permit_allowlisted.tz"))

auctionTezPermitAllowlistedTokenContract
  :: Contract
      (AuctionTezPermit.PermitAuctionEntrypoints AllowlistToken.Entrypoints)
      (AuctionTezPermit.PermitAuctionStorage AllowlistToken.Allowlist)
auctionTezPermitAllowlistedTokenContract =
  $$(embedContractM (inBinFolder "english_auction_tez_permit_allowlisted_token.tz"))
