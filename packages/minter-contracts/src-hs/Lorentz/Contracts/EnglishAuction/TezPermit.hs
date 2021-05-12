-- | Lorentz bindings for the english auction (Tez with permit version).
module Lorentz.Contracts.EnglishAuction.TezPermit where

import Lorentz

import Lorentz.Contracts.MinterSdk
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

data PermitAuctionStorage = PermitAuctionStorage
  { auctionStorage :: AuctionStorage
  , counter :: Natural
  }

customGeneric "PermitAuctionStorage" ligoCombLayout
deriving anyclass instance IsoValue PermitAuctionStorage
deriving anyclass instance HasAnnotation PermitAuctionStorage

initPermitAuctionStorage :: AdminStorage -> PermitAuctionStorage
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

data PermitAuctionEntrypoints
  = AdminAndInteract AuctionWithoutConfigureEntrypoints
  | Permit_configure [PermitConfigParam]

customGeneric "PermitAuctionEntrypoints" ligoLayout
deriving anyclass instance IsoValue PermitAuctionEntrypoints
deriving anyclass instance HasAnnotation PermitAuctionEntrypoints

instance ParameterHasEntrypoints PermitAuctionEntrypoints where
  type ParameterEntrypointsDerivation PermitAuctionEntrypoints = EpdRecursive

-- Contract
----------------------------------------------------------------------------

auctionTezPermitContract :: T.Contract (ToT PermitAuctionEntrypoints) (ToT PermitAuctionStorage)
auctionTezPermitContract = $$(embedContractM (inBinFolder "english_auction_tez_permit.tz"))

-- Errors
----------------------------------------------------------------------------
