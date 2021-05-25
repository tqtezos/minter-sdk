-- | Lorentz bindings for the english auction (Tez + Fixed Fee version).
module Lorentz.Contracts.EnglishAuction.TezFixedFee
  ( -- * Types
    AuctionTez.Auction(..)
  , AuctionTez.ConfigureParam(..)
  , AuctionTez.defConfigureParam
  , AuctionStorage(..)
  , initAuctionStorage
  , AuctionTez.AuctionWithoutConfigureEntrypoints(..)
  , AuctionTez.AuctionEntrypoints(..)

  -- * Contract
  , englishAuctionTezFixedFeeContract
  ) where

import Lorentz

import Fmt (Buildable(..), genericF)
import Lorentz.Contracts.EnglishAuction.Common
import qualified Lorentz.Contracts.EnglishAuction.Tez as AuctionTez
import Lorentz.Contracts.MinterSdk
import qualified Lorentz.Contracts.NoAllowlist as NoAllowlist
import Lorentz.Contracts.PausableAdminOption
import Michelson.Test.Import (embedContractM)
import qualified Michelson.Typed as T

-- Types
----------------------------------------------------------------------------

data AuctionStorage = AuctionStorage
  { pausableAdmin :: AdminStorage
  , currentId :: Natural
  , maxAuctionTime :: Natural
  , maxConfigToStartTime :: Natural
  , auctions :: BigMap AuctionId AuctionTez.Auction
  , allowlist :: ()
  , fee :: FeeData
  }

customGeneric "AuctionStorage" ligoCombLayout
deriving anyclass instance IsoValue AuctionStorage
deriving anyclass instance HasAnnotation AuctionStorage
instance Buildable AuctionStorage where build = genericF

initAuctionStorage :: FeeData -> AdminStorage -> AuctionStorage
initAuctionStorage feeData as = AuctionStorage
  { pausableAdmin = as
  , currentId = 0
  , maxAuctionTime = 99999999999999999999
  , maxConfigToStartTime = 99999999999999999
  , auctions = mempty
  , allowlist = ()
  , fee = feeData
  }

-- Contract
----------------------------------------------------------------------------

englishAuctionTezFixedFeeContract
  :: T.Contract
      (ToT (AuctionTez.AuctionEntrypoints NoAllowlist.Entrypoints))
      (ToT AuctionStorage)
englishAuctionTezFixedFeeContract = $$(embedContractM (inBinFolder "english_auction_tez_fixed_fee.tz"))
