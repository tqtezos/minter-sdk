module Lorentz.Contracts.EnglishAuction.ConsolationOffchain where

import Lorentz
import Fmt (Buildable(..), genericF)
import qualified Lorentz.Contracts.EnglishAuction.Common as Common
import qualified Lorentz.Contracts.EnglishAuction.Consolation as Consolation
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2I
import Lorentz.Contracts.PausableAdminOption
import qualified Lorentz.Contracts.EnglishAuction.TezPermit as TezPermit
import qualified Michelson.Typed as T
import qualified Lorentz.Contracts.NoAllowlist as NoAllowlist
import Michelson.Test.Import (embedContractM)
import Lorentz.Contracts.MinterSdk

data Auction = Auction
  { seller :: Address
  , currentBid :: Mutez
  , startTime :: Timestamp
  , lastBidTime :: Timestamp
  , roundTime  :: Integer
  , extendTime  :: Integer
  , asset :: [Common.Tokens]
  , minRaisePercent :: Natural
  , minRaise :: Mutez
  , endTime :: Timestamp
  , highestBidder :: Address
  , consolationWinners :: Consolation.ConsolationWinnerData
  , consolationToken :: Consolation.GlobalTokenId
  , maxConsolationWinners :: Natural
  , lastBidOffchain :: Bool
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
  , asset :: [Common.Tokens]
  , startTime :: Timestamp
  , endTime :: Timestamp
  , consolationToken :: Consolation.GlobalTokenId
  , maxConsolationWinners :: Natural
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
  , consolationToken = Consolation.GlobalTokenId Consolation.exampleFA2Address (FA2I.TokenId 0)
  , maxConsolationWinners = 10
  }

data AuctionStorage al = AuctionStorage
  { pausableAdmin :: AdminStorage
  , currentId :: Natural
  , maxAuctionTime :: Natural
  , maxConfigToStartTime :: Natural
  , auctions :: BigMap Common.AuctionId Auction
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

data OffchainBidData = OffchainBidData
  { assetId :: Natural
  , bidAmount :: Mutez 
  } deriving stock (Eq, Ord)

customGeneric "OffchainBidData" ligoCombLayout
deriving anyclass instance IsoValue OffchainBidData
deriving anyclass instance HasAnnotation OffchainBidData
instance Buildable OffchainBidData where build = genericF

data OffchainBidParam = OffchainBidParam
  { offchainBidData :: OffchainBidData
  , permit :: TezPermit.Permit
  } deriving stock (Eq, Ord)

customGeneric "OffchainBidParam" ligoCombLayout
deriving anyclass instance IsoValue OffchainBidParam
deriving anyclass instance HasAnnotation OffchainBidParam
instance Buildable OffchainBidParam where build = genericF

data AuctionWithoutConfigureEntrypoints al
  = Bid Common.AuctionId
  | Cancel Common.AuctionId
  | Resolve Common.AuctionId
  | Admin AdminEntrypoints
  | Update_allowed al
  | Offchain_bid OffchainBidParam
  | Send_consolation (Common.AuctionId, [Natural])

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


-- Contract
----------------------------------------------------------------------------

consolationAuctionTezContract
  :: T.Contract
      (ToT (AuctionEntrypoints NoAllowlist.Entrypoints))
      (ToT (AuctionStorage NoAllowlist.Allowlist))
consolationAuctionTezContract =
  $$(embedContractM (inBinFolder "english_auction_tez_offchain_bid_consolation_auction.tz"))

-- Errors
----------------------------------------------------------------------------

errNotAdmin :: MText
errNotAdmin = [mt|NOT_AN_ADMIN|]

errAuctionNotEnded :: MText 
errAuctionNotEnded = [mt|AUCTION_NOT_ENDED|]