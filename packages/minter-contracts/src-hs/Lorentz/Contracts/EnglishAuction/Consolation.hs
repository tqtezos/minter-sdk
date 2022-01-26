-- | Lorentz bindings for the english auction (Tez version).
module Lorentz.Contracts.EnglishAuction.Consolation where

import Lorentz

import Fmt (Buildable(..), genericF)
import Lorentz.Contracts.EnglishAuction.Common
import Lorentz.Contracts.PausableAdminOption
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2I
import Tezos.Address (unsafeParseAddress)
-- Types
----------------------------------------------------------------------------

data GlobalTokenId = GlobalTokenId
  { fa2Address :: Address
  , tokenId :: FA2I.TokenId
  } deriving stock (Eq, Ord)

customGeneric "GlobalTokenId" ligoCombLayout
deriving anyclass instance IsoValue GlobalTokenId
deriving anyclass instance HasAnnotation GlobalTokenId
instance Buildable GlobalTokenId where build = genericF

data ConsolationWinnerArray = ConsolationWinnerArray
  {
    size :: Natural 
  , bidIndex :: Natural 
  , content :: Map Natural Address 
  }

customGeneric "ConsolationWinnerArray" ligoCombLayout
deriving anyclass instance IsoValue ConsolationWinnerArray
deriving anyclass instance HasAnnotation ConsolationWinnerArray
instance Buildable ConsolationWinnerArray where build = genericF

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
  , consolationWinners :: ConsolationWinnerArray
  , consolationToken :: GlobalTokenId
  , maxConsolationWinners :: Natural
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
  , consolationToken :: GlobalTokenId
  , maxConsolationWinners :: Natural
  }

exampleFA2Address :: Address 
exampleFA2Address = unsafeParseAddress "KT1T7ShxhtSRuhvhHeug6Sjc7W8irLmswEt7"

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
  , consolationToken = GlobalTokenId exampleFA2Address (FA2I.TokenId 0)
  , maxConsolationWinners = 10
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
  | Send_consolation (AuctionId, [Natural])

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
