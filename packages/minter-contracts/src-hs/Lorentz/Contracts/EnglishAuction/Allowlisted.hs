-- | Lorentz bindings for the fixed price sale contract
-- with allowlist.
module Lorentz.Contracts.EnglishAuction.Allowlisted where

import Lorentz

import Michelson.Test.Import (embedContractM)
import qualified Michelson.Typed as T

import qualified Lorentz.Contracts.EnglishAuction.FA2 as FA2
import qualified Lorentz.Contracts.EnglishAuction.Tez as Tez
import qualified Lorentz.Contracts.EnglishAuction.TezPermit as TezPermit
import Lorentz.Contracts.MinterSdk

-- Types
----------------------------------------------------------------------------

data AllowlistedStorage auctionStorage = AllowlistedStorage
  { auctionStorage :: auctionStorage
  , allowlist :: BigMap Address ()
  }

customGeneric "AllowlistedStorage" ligoLayout
deriving anyclass instance
  IsoValue s =>
  IsoValue (AllowlistedStorage s)
deriving anyclass instance
  HasAnnotation s =>
  HasAnnotation (AllowlistedStorage s)

initAllowlistedStorage :: auctionStorage -> AllowlistedStorage auctionStorage
initAllowlistedStorage ms = AllowlistedStorage ms mempty

data AllowlistedEntrypoints auctionEntrypoints
  = Call_auction auctionEntrypoints
  | Update_allowed (BigMap Address ())

customGeneric "AllowlistedEntrypoints" ligoCombLayout
deriving anyclass instance
  IsoValue ep =>
  IsoValue (AllowlistedEntrypoints ep)
deriving anyclass instance
  HasAnnotation ep =>
  HasAnnotation (AllowlistedEntrypoints ep)

instance ParameterHasEntrypoints (AllowlistedEntrypoints FA2.AuctionEntrypoints) where
  type ParameterEntrypointsDerivation (AllowlistedEntrypoints FA2.AuctionEntrypoints) = EpdRecursive

instance ParameterHasEntrypoints (AllowlistedEntrypoints Tez.AuctionEntrypoints) where
  type ParameterEntrypointsDerivation (AllowlistedEntrypoints Tez.AuctionEntrypoints) = EpdRecursive

instance ParameterHasEntrypoints (AllowlistedEntrypoints TezPermit.PermitAuctionEntrypoints) where
  type ParameterEntrypointsDerivation (AllowlistedEntrypoints TezPermit.PermitAuctionEntrypoints) = EpdRecursive

-- Contract
----------------------------------------------------------------------------

auctionFA2AllowlistedContract
  :: T.Contract
      (ToT (AllowlistedEntrypoints FA2.AuctionEntrypoints))
      (ToT (AllowlistedStorage FA2.AuctionStorage))
auctionFA2AllowlistedContract =
  $$(embedContractM (inBinFolder "english_auction_fa2_allowlisted.tz"))

auctionTezAllowlistedContract
  :: T.Contract
      (ToT (AllowlistedEntrypoints Tez.AuctionEntrypoints))
      (ToT (AllowlistedStorage Tez.AuctionStorage))
auctionTezAllowlistedContract =
  $$(embedContractM (inBinFolder "english_auction_tez_allowlisted.tz"))

auctionTezPermitAllowlistedContract
  :: T.Contract
      (ToT (AllowlistedEntrypoints TezPermit.PermitAuctionEntrypoints))
      (ToT (AllowlistedStorage TezPermit.PermitAuctionStorage))
auctionTezPermitAllowlistedContract =
  $$(embedContractM (inBinFolder "english_auction_tez_permit_allowlisted.tz"))

-- Errors
----------------------------------------------------------------------------

assetAddressNotAllowed :: MText
assetAddressNotAllowed = [mt|ASSET_ADDRESS_NOT_ALLOWED|]
