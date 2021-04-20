-- | Lorentz bindings for the english auction (Tez with permit version).
module Lorentz.Contracts.EnglishAuction.TezPermit where

import Lorentz

import qualified Lorentz.Contracts.AllowlistSimple as AllowlistSimple
import Lorentz.Contracts.MinterSdk
import qualified Lorentz.Contracts.NoAllowlist as NoAllowlist
import Lorentz.Contracts.PausableAdminOption
import Michelson.Test.Import (embedContractM)
import qualified Michelson.Typed as T

import Lorentz.Contracts.EnglishAuction.Tez

-- Types
----------------------------------------------------------------------------

data Permit = Permit
  { signerKey :: PublicKey
  , signature :: Signature
  }

customGeneric "Permit" ligoCombLayout
deriving anyclass instance IsoValue Permit
deriving anyclass instance HasAnnotation Permit

data PermitAuctionStorage al = PermitAuctionStorage
  { auctionStorage :: AuctionStorage al
  , counter :: Natural
  }

customGeneric "PermitAuctionStorage" ligoCombLayout
deriving anyclass instance IsoValue al => IsoValue (PermitAuctionStorage al)
deriving anyclass instance HasAnnotation al => HasAnnotation (PermitAuctionStorage al)

initPermitAuctionStorage :: Monoid al => AdminStorage -> PermitAuctionStorage al
initPermitAuctionStorage as = PermitAuctionStorage
  { auctionStorage = initAuctionStorage as
  , counter = 0
  }

data PermitConfigParam = PermitConfigParam
  { config :: ConfigureParam
  , optionalPermit :: Maybe Permit
  }

customGeneric "PermitConfigParam" ligoCombLayout
deriving anyclass instance IsoValue PermitConfigParam
deriving anyclass instance HasAnnotation PermitConfigParam

data PermitAuctionEntrypoints al
  = AdminAndInteract (AuctionWithoutConfigureEntrypoints al)
  | Permit_configure [PermitConfigParam]

customGeneric "PermitAuctionEntrypoints" ligoLayout
deriving anyclass instance IsoValue al => IsoValue (PermitAuctionEntrypoints al)
deriving anyclass instance HasAnnotation al => HasAnnotation (PermitAuctionEntrypoints al)

instance
  ( RequireAllUniqueEntrypoints (PermitAuctionEntrypoints al), IsoValue al
  , EntrypointsDerivation EpdDelegate (PermitAuctionEntrypoints al)
  ) =>
  ParameterHasEntrypoints (PermitAuctionEntrypoints al) where
  type ParameterEntrypointsDerivation (PermitAuctionEntrypoints al) = EpdDelegate

-- Contract
----------------------------------------------------------------------------

auctionTezPermitContract
  :: T.Contract
      (ToT (PermitAuctionEntrypoints NoAllowlist.Entrypoints))
      (ToT (PermitAuctionStorage NoAllowlist.Allowlist))
auctionTezPermitContract =
  $$(embedContractM (inBinFolder "english_auction_tez_permit.tz"))

auctionTezPermitAllowlistedContract
  :: T.Contract
      (ToT (PermitAuctionEntrypoints AllowlistSimple.Entrypoints))
      (ToT (PermitAuctionStorage AllowlistSimple.Allowlist))
auctionTezPermitAllowlistedContract =
  $$(embedContractM (inBinFolder "english_auction_tez_permit_allowlisted.tz"))

-- Errors
----------------------------------------------------------------------------
