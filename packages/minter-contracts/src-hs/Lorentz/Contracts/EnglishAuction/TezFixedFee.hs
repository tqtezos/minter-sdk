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
  ) where

import Lorentz

import Fmt (Buildable(..), genericF)
import Lorentz.Contracts.EnglishAuction.Common
import qualified Lorentz.Contracts.EnglishAuction.Tez as AuctionTez
import Lorentz.Contracts.MinterSdk
import Lorentz.Contracts.PausableAdminOption

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
