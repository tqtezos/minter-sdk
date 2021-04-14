-- | Lorentz bindings for the english auction (FA2 version).
module Lorentz.Contracts.EnglishAuction.FA2 where

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

data BidCurrency = BidCurrency
  { fa2Address :: Address
  , tokenId :: TokenId
  }

customGeneric "BidCurrency" ligoCombLayout
deriving anyclass instance IsoValue BidCurrency
deriving anyclass instance HasAnnotation BidCurrency

data Auction = Auction
  { seller :: Address
  , currentBid :: Natural
  , startTime :: Timestamp
  , lastBidTime :: Timestamp
  , roundTime  :: Integer
  , extendTime  :: Integer
  , asset :: [Tokens]
  , minRaisePercent :: Natural
  , minRaise :: Natural
  , endTime :: Timestamp
  , highestBidder :: Address
  }

customGeneric "Auction" ligoCombLayout
deriving anyclass instance IsoValue Auction
deriving anyclass instance HasAnnotation Auction

data ConfigureParam = ConfigureParam
  { openingPrice :: Natural
  , minRaisePercent :: Natural
  , minRaise :: Natural
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
  { openingPrice = 1
  , minRaisePercent = 1
  , minRaise = 1
  , roundTime = 100000000000
  , extendTime = 100000000000
  , asset = []
  , startTime = timestampFromSeconds 0
  , endTime = timestampFromSeconds 1000000000000
  }

data BidParam = BidParam
  { assetId :: AuctionId
  , bidAmount :: Natural
  }

customGeneric "BidParam" ligoCombLayout
deriving anyclass instance IsoValue BidParam
deriving anyclass instance HasAnnotation BidParam

data AuctionStorage = AuctionStorage
  { pausableAdmin :: AdminStorage
  , currentId :: Natural
  , maxAuctionTime :: Natural
  , maxConfigToStartTime :: Natural
  , bidCurrency :: BidCurrency
  , auctions :: BigMap Natural Auction
  }

customGeneric "AuctionStorage" ligoCombLayout
deriving anyclass instance IsoValue AuctionStorage
deriving anyclass instance HasAnnotation AuctionStorage

initAuctionStorage :: AdminStorage -> BidCurrency -> AuctionStorage
initAuctionStorage as bc = AuctionStorage
  { pausableAdmin = as
  , currentId = 0
  , maxAuctionTime = 99999999999999999999
  , maxConfigToStartTime = 99999999999999999
  , bidCurrency = bc
  , auctions = mempty
  }

data AuctionEntrypoints
  = Configure ConfigureParam
  | Bid BidParam
  | Cancel AuctionId
  | Resolve AuctionId
  | Admin AdminEntrypoints

customGeneric "AuctionEntrypoints" ligoLayout
deriving anyclass instance IsoValue AuctionEntrypoints
deriving anyclass instance HasAnnotation AuctionEntrypoints

instance ParameterHasEntrypoints AuctionEntrypoints where
  type ParameterEntrypointsDerivation AuctionEntrypoints = EpdRecursive

-- This empty slice is a workaround, so that all the declarations above and
-- their instances may be in the type environment in the TH splice below.
$(pure [])

-- Contract
----------------------------------------------------------------------------

englishAuctionFA2Contract :: T.Contract (ToT AuctionEntrypoints) (ToT AuctionStorage)
englishAuctionFA2Contract = $$(embedContractM (inBinFolder "english_auction_fa2.tz"))

-- Errors
----------------------------------------------------------------------------
