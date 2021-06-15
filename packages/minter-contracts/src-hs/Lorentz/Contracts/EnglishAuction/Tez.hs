-- | Lorentz bindings for the english auction (Tez version).
module Lorentz.Contracts.EnglishAuction.Tez where

import Lorentz

import Fmt (Buildable(..), genericF)
import Lorentz.Contracts.EnglishAuction.Common
import Lorentz.Contracts.PausableAdminOption

-- Types
----------------------------------------------------------------------------

data Auction = Auction
  { seller :: Address
  , currentBid :: Mutez
  , startTime :: Timestamp
  , lastBidTime :: Timestamp
  , roundTime  :: Integer
  , extendTime  :: Integer
  , asset :: [Tokens]
  , minRaisePercent :: Natural
  , minRaise :: Mutez
  , endTime :: Timestamp
  , highestBidder :: Address
  }

customGeneric "Auction" ligoCombLayout
deriving anyclass instance IsoValue Auction
deriving anyclass instance HasAnnotation Auction
instance Buildable Auction where build = genericF

data ConfigureParam = ConfigureParam
  { openingPrice :: Mutez
  , minRaisePercent :: Natural
  , minRaise :: Mutez
  , roundTime :: Natural
  , extendTime :: Natural
  , asset :: [Tokens]
  , startTime :: Timestamp
  , endTime :: Timestamp
  }

customGeneric "ConfigureParam" ligoCombLayout
deriving anyclass instance IsoValue ConfigureParam
deriving anyclass instance HasAnnotation ConfigureParam

defConfigureParam :: ConfigureParam
defConfigureParam = ConfigureParam
  { openingPrice = toMutez 1
  , minRaisePercent = 1
  , minRaise = toMutez 1
  , roundTime = 100000000000
  , extendTime = 100000000000
  , asset = []
  , startTime = timestampFromSeconds 0
  , endTime = timestampFromSeconds 1000000000000
  }

data AuctionStorage al = AuctionStorage
  { pausableAdmin :: AdminStorage
  , currentId :: Natural
  , maxAuctionTime :: Natural
  , maxConfigToStartTime :: Natural
  , auctions :: BigMap AuctionId Auction
  , allowlist :: al
  }

customGeneric "AuctionStorage" ligoCombLayout
deriving anyclass instance IsoValue al => IsoValue (AuctionStorage al)
deriving anyclass instance HasAnnotation al => HasAnnotation (AuctionStorage al)

initAuctionStorage :: Monoid al => AdminStorage -> AuctionStorage al
initAuctionStorage as = AuctionStorage
  { pausableAdmin = as
  , currentId = 0
  , maxAuctionTime = 99999999999999999999
  , maxConfigToStartTime = 99999999999999999
  , auctions = mempty
  , allowlist = mempty
  }

data AuctionWithoutConfigureEntrypoints al
  = Bid AuctionId
  | Cancel AuctionId
  | Resolve AuctionId
  | Admin AdminEntrypoints
  | Update_allowed al

customGeneric "AuctionWithoutConfigureEntrypoints" ligoLayout
deriving anyclass instance IsoValue al => IsoValue (AuctionWithoutConfigureEntrypoints al)
deriving anyclass instance HasAnnotation al => HasAnnotation (AuctionWithoutConfigureEntrypoints al)

instance
  ( RequireAllUniqueEntrypoints (AuctionWithoutConfigureEntrypoints al), IsoValue al
  , EntrypointsDerivation EpdDelegate (AuctionWithoutConfigureEntrypoints al)
  ) =>
    ParameterHasEntrypoints (AuctionWithoutConfigureEntrypoints al) where
  type ParameterEntrypointsDerivation (AuctionWithoutConfigureEntrypoints al) = EpdDelegate

data AuctionEntrypoints al
  = Configure ConfigureParam
  | AdminAndInteract (AuctionWithoutConfigureEntrypoints al)

customGeneric "AuctionEntrypoints" ligoLayout
deriving anyclass instance IsoValue al => IsoValue (AuctionEntrypoints al)
deriving anyclass instance HasAnnotation al => HasAnnotation (AuctionEntrypoints al)

instance
  ( RequireAllUniqueEntrypoints (AuctionEntrypoints al), IsoValue al
  , EntrypointsDerivation EpdDelegate (AuctionEntrypoints al)
  ) =>
    ParameterHasEntrypoints (AuctionEntrypoints al) where
  type ParameterEntrypointsDerivation (AuctionEntrypoints al) = EpdDelegate

-- Errors
----------------------------------------------------------------------------
