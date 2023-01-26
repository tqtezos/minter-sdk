module Lorentz.Contracts.MultiunitAuction.Auction where

import Lorentz

import Fmt (Buildable(..), genericF)
import qualified Lorentz.Contracts.AllowlistSimple as AllowlistSimple
import qualified Lorentz.Contracts.AllowlistToken as AllowlistToken
import qualified Lorentz.Contracts.EnglishAuction.TezPermit as Permit
import Lorentz.Contracts.MinterSdk
import qualified Lorentz.Contracts.NoAllowlist as NoAllowlist
import Lorentz.Contracts.PausableAdminOption
import Lorentz.Test.Import (embedContractM)
import qualified Michelson.Typed as T
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2I

-- Types
----------------------------------------------------------------------------

newtype AuctionId = AuctionId Natural
  deriving stock (Generic, Eq, Ord, Show)
  deriving newtype (IsoValue, HasAnnotation, Buildable)

data Auction = Auction
  { seller :: Address
  , priceFloor :: Mutez
  , startTime :: Timestamp
  , lastBidTime :: Timestamp
  , roundTime  :: Integer
  , extendTime  :: Integer
  , fa2Address :: Address
  , endTime :: Timestamp
  , bondingCurve :: Natural
  , bidIndex :: Natural
  , numOffersToPayoutOrReturn :: Natural
  , numWinningOffers :: Maybe Natural
  , winningPrice :: Maybe Mutez
  , isCanceled :: Bool
  , nextTokenId :: Maybe Natural 
  , profitAddress :: Address
  , highestOfferPrice :: Mutez  
  , tokenInfo :: FA2I.TokenMetadata
  }

customGeneric "Auction" ligoCombLayout
deriving anyclass instance IsoValue Auction
deriving anyclass instance HasAnnotation Auction

data ConfigureParam = ConfigureParam
  { priceFloor :: Mutez
  , roundTime :: Natural
  , extendTime :: Natural
  , fa2Address :: Address
  , startTime :: Timestamp 
  , endTime :: Timestamp
  , bondingCurve :: Natural 
  , profitAddress :: Address 
  , tokenInfo :: FA2I.TokenMetadata
  }

customGeneric "ConfigureParam" ligoCombLayout
deriving anyclass instance IsoValue ConfigureParam
deriving anyclass instance HasAnnotation ConfigureParam

data BidHeapKey = BidHeapKey 
  {
    auctionId :: AuctionId 
  , bidIndex :: Natural  
  } deriving stock (Eq, Ord, Show)

customGeneric "BidHeapKey" ligoCombLayout
deriving anyclass instance IsoValue BidHeapKey
deriving anyclass instance HasAnnotation BidHeapKey

data BidData = BidData 
  {
    price :: Mutez
  , bidder :: Address
  , quantity :: Natural
  , isOffchain :: Bool
  , bidTime :: Timestamp
  }

customGeneric "BidData" ligoCombLayout
deriving anyclass instance IsoValue BidData
deriving anyclass instance HasAnnotation BidData

data BidParam = BidParam 
 {  
    auctionId :: Natural
  , priceParam :: Mutez
  , quantityParam :: Natural
  , isBidIncrease :: Maybe BidHeapKey
 } deriving stock (Eq, Ord, Show)

customGeneric "BidParam" ligoCombLayout
deriving anyclass instance IsoValue BidParam
deriving anyclass instance HasAnnotation BidParam

data PermitMultiunitBidParam = PermitMultiunitBidParam
  {
    bidParam :: BidParam
  , permit :: Permit.Permit
  }

customGeneric "PermitMultiunitBidParam" ligoCombLayout
deriving anyclass instance IsoValue PermitMultiunitBidParam
deriving anyclass instance HasAnnotation PermitMultiunitBidParam

data AuctionStorage = AuctionStorage
  { pausableAdmin :: AdminStorage
  , auctionId :: AuctionId
  , maxAuctionTime :: Natural
  , maxConfigToStartTime :: Natural
  , auctions :: BigMap AuctionId Auction
  , bondingCurveIndex :: Natural
  , bondingCurves :: BigMap Natural ('[Natural] :-> '[Mutez])
  , bids :: BigMap BidHeapKey BidData
  , heapSizes :: BigMap AuctionId Natural
  }

customGeneric "AuctionStorage" ligoCombLayout
deriving anyclass instance IsoValue AuctionStorage
deriving anyclass instance HasAnnotation AuctionStorage


initAuctionStorage :: AdminStorage -> AuctionStorage
initAuctionStorage as = AuctionStorage
  { pausableAdmin = as
  , auctionId = AuctionId 0
  , maxAuctionTime = 99999999999999999999
  , maxConfigToStartTime = 99999999999999999
  , auctions = mempty
  , bondingCurveIndex = 0 
  , bondingCurves = mempty 
  , bids = mempty 
  , heapSizes = mempty
  }

data AuctionWithoutConfigureEntrypoints
  = Bid BidParam
  | Cancel AuctionId
  | Resolve AuctionId
  | Admin AdminEntrypoints
  | Offchain_Bid PermitMultiunitBidParam
  | Return_old_bids (AuctionId, Natural)
  | Return_old_offers (AuctionId, Natural)
  | Payout_winners (AuctionId, Natural)
  | Add_bonding_curve ('[Natural] :-> '[Mutez])

customGeneric "AuctionWithoutConfigureEntrypoints" ligoLayout
deriving anyclass instance IsoValue AuctionWithoutConfigureEntrypoints
deriving anyclass instance HasAnnotation AuctionWithoutConfigureEntrypoints

instance
  ( RequireAllUniqueEntrypoints AuctionWithoutConfigureEntrypoints
  , EntrypointsDerivation EpdDelegate AuctionWithoutConfigureEntrypoints
  ) =>
    ParameterHasEntrypoints AuctionWithoutConfigureEntrypoints where
  type ParameterEntrypointsDerivation AuctionWithoutConfigureEntrypoints = EpdDelegate

data AuctionEntrypoints
  = Configure ConfigureParam
  | AdminAndInteract AuctionWithoutConfigureEntrypoints

customGeneric "AuctionEntrypoints" ligoLayout
deriving anyclass instance IsoValue AuctionEntrypoints
deriving anyclass instance HasAnnotation AuctionEntrypoints

instance
  ( RequireAllUniqueEntrypoints AuctionEntrypoints
  , EntrypointsDerivation EpdDelegate AuctionEntrypoints
  ) =>
    ParameterHasEntrypoints AuctionEntrypoints where
  type ParameterEntrypointsDerivation AuctionEntrypoints = EpdDelegate

-- Errors
----------------------------------------------------------------------------
