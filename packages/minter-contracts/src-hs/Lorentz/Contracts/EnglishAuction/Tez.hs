-- | Lorentz bindings for the english auction (Tez version).
module Lorentz.Contracts.EnglishAuction.Tez where

import Lorentz

import Lorentz.Contracts.MinterSdk
import Lorentz.Contracts.PausableAdminOption
import Lorentz.Contracts.Spec.FA2Interface
import Michelson.Test.Import (embedContractM)
import qualified Michelson.Typed as T

-- Types
----------------------------------------------------------------------------

newtype AuctionId = AuctionId Natural
  deriving stock (Generic, Eq, Ord)
  deriving newtype (IsoValue, HasAnnotation)

data FA2Token = FA2Token
  { tokenId :: TokenId
  , amount :: Natural
  }

customGeneric "FA2Token" ligoCombLayout
deriving anyclass instance IsoValue FA2Token
deriving anyclass instance HasAnnotation FA2Token

data Tokens = Tokens
  { fa2Address :: Address
  , fa2Batch :: [FA2Token]
  }

customGeneric "Tokens" ligoCombLayout
deriving anyclass instance IsoValue Tokens
deriving anyclass instance HasAnnotation Tokens

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

data AuctionStorage = AuctionStorage
  { pausableAdmin :: AdminStorage
  , currentId :: Natural
  , maxAuctionTime :: Natural
  , maxConfigToStartTime :: Natural
  , auctions :: BigMap Natural Auction
  }

customGeneric "AuctionStorage" ligoCombLayout
deriving anyclass instance IsoValue AuctionStorage
deriving anyclass instance HasAnnotation AuctionStorage

initAuctionStorage :: AdminStorage -> AuctionStorage
initAuctionStorage as = AuctionStorage
  { pausableAdmin = as
  , currentId = 0
  , maxAuctionTime = 99999999999999999999
  , maxConfigToStartTime = 99999999999999999
  , auctions = mempty
  }

data AuctionWithoutConfigureEntrypoints
  = Bid AuctionId
  | Cancel AuctionId
  | Resolve AuctionId
  | Admin AdminEntrypoints

customGeneric "AuctionWithoutConfigureEntrypoints" ligoLayout
deriving anyclass instance IsoValue AuctionWithoutConfigureEntrypoints
deriving anyclass instance HasAnnotation AuctionWithoutConfigureEntrypoints

data AuctionEntrypoints
  = Configure ConfigureParam
  | AdminAndInteract AuctionWithoutConfigureEntrypoints

customGeneric "AuctionEntrypoints" ligoLayout
deriving anyclass instance IsoValue AuctionEntrypoints
deriving anyclass instance HasAnnotation AuctionEntrypoints

instance ParameterHasEntrypoints AuctionEntrypoints where
  type ParameterEntrypointsDerivation AuctionEntrypoints = EpdRecursive

-- Contract
----------------------------------------------------------------------------

englishAuctionTezContract :: T.Contract (ToT AuctionEntrypoints) (ToT AuctionStorage)
englishAuctionTezContract = $$(embedContractM (inBinFolder "english_auction_tez.tz"))

-- Errors
----------------------------------------------------------------------------
