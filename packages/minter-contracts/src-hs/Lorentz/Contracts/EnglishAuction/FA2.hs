-- | Lorentz bindings for the english auction (FA2 version).
module Lorentz.Contracts.EnglishAuction.FA2 where

import Lorentz

import Fmt (Buildable(..), genericF)
import qualified Lorentz.Contracts.AllowlistSimple as AllowlistSimple
import qualified Lorentz.Contracts.AllowlistToken as AllowlistToken
import Lorentz.Contracts.EnglishAuction.Common
import Lorentz.Contracts.MinterSdk
import qualified Lorentz.Contracts.NoAllowlist as NoAllowlist
import Lorentz.Contracts.PausableAdminOption
import Lorentz.Contracts.Spec.FA2Interface
import Michelson.Test.Import (embedContractM)
import qualified Michelson.Typed as T

-- Types
----------------------------------------------------------------------------

data BidCurrency = BidCurrency
  { fa2Address :: Address
  , tokenId :: TokenId
  }

customGeneric "BidCurrency" ligoCombLayout
deriving anyclass instance IsoValue BidCurrency
deriving anyclass instance HasAnnotation BidCurrency
instance Buildable BidCurrency where build = genericF

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
instance Buildable Auction where build = genericF

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

data AuctionStorage al = AuctionStorage
  { pausableAdmin :: AdminStorage
  , currentId :: Natural
  , maxAuctionTime :: Natural
  , maxConfigToStartTime :: Natural
  , bidCurrency :: BidCurrency
  , auctions :: BigMap AuctionId Auction
  , allowlist :: al
  }

customGeneric "AuctionStorage" ligoCombLayout
deriving anyclass instance IsoValue al => IsoValue (AuctionStorage al)
deriving anyclass instance HasAnnotation al => HasAnnotation (AuctionStorage al)

initAuctionStorage :: Monoid al => AdminStorage -> BidCurrency -> AuctionStorage al
initAuctionStorage as bc = AuctionStorage
  { pausableAdmin = as
  , currentId = 0
  , maxAuctionTime = 99999999999999999999
  , maxConfigToStartTime = 99999999999999999
  , bidCurrency = bc
  , auctions = mempty
  , allowlist = mempty
  }

data AuctionEntrypoints al
  = Configure ConfigureParam
  | Bid BidParam
  | Cancel AuctionId
  | Resolve AuctionId
  | Admin AdminEntrypoints
  | Update_allowed al

customGeneric "AuctionEntrypoints" ligoLayout
deriving anyclass instance IsoValue al => IsoValue (AuctionEntrypoints al)
deriving anyclass instance HasAnnotation al => HasAnnotation (AuctionEntrypoints al)

instance
  ( RequireAllUniqueEntrypoints (AuctionEntrypoints al), IsoValue al
  , EntrypointsDerivation EpdDelegate (AuctionEntrypoints al)
  ) =>
    ParameterHasEntrypoints (AuctionEntrypoints al) where
  type ParameterEntrypointsDerivation (AuctionEntrypoints al) = EpdDelegate

-- Contract
----------------------------------------------------------------------------

englishAuctionFA2Contract
  :: T.Contract
      (ToT (AuctionEntrypoints NoAllowlist.Entrypoints))
      (ToT (AuctionStorage NoAllowlist.Allowlist))
englishAuctionFA2Contract =
  $$(embedContractM (inBinFolder "english_auction_fa2.tz"))

auctionFA2AllowlistedContract
  :: T.Contract
      (ToT (AuctionEntrypoints AllowlistSimple.Entrypoints))
      (ToT (AuctionStorage AllowlistSimple.Allowlist))
auctionFA2AllowlistedContract =
  $$(embedContractM (inBinFolder "english_auction_fa2_allowlisted.tz"))

auctionFA2AllowlistedTokenContract
  :: T.Contract
      (ToT (AuctionEntrypoints AllowlistToken.Entrypoints))
      (ToT (AuctionStorage AllowlistToken.Allowlist))
auctionFA2AllowlistedTokenContract =
  $$(embedContractM (inBinFolder "english_auction_fa2_allowlisted_token.tz"))

-- Errors
----------------------------------------------------------------------------
