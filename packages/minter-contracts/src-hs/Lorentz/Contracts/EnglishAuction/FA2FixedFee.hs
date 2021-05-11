-- | Lorentz bindings for the english auction (FA2 + Fixed Fee version).
module Lorentz.Contracts.EnglishAuction.FA2FixedFee
  ( -- * Types
    AuctionFA2.AuctionId(..)
  , AuctionFA2.FA2Token(..)
  , AuctionFA2.Tokens(..)
  , AuctionFA2.BidCurrency(..)
  , AuctionFA2.Auction(..)
  , AuctionFA2.ConfigureParam(..)
  , AuctionFA2.BidParam(..)
  , AuctionFA2.defConfigureParam
  , AuctionStorage(..)
  , initAuctionStorage
  , AuctionFA2.AuctionEntrypoints(..)

  -- * Contract
  , englishAuctionFA2FixedFeeContract
  ) where

import Lorentz

import Fmt (Buildable(..), genericF)
import qualified Lorentz.Contracts.EnglishAuction.FA2 as AuctionFA2
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
  , bidCurrency :: AuctionFA2.BidCurrency
  , auctions :: BigMap AuctionFA2.AuctionId AuctionFA2.Auction
  , allowlist :: ()
  , fee :: FeeData
  }

customGeneric "AuctionStorage" ligoCombLayout
deriving anyclass instance IsoValue AuctionStorage
deriving anyclass instance HasAnnotation AuctionStorage
instance Buildable AuctionStorage where build = genericF

initAuctionStorage :: FeeData -> AdminStorage -> AuctionFA2.BidCurrency -> AuctionStorage
initAuctionStorage feeData as bc = AuctionStorage
  { pausableAdmin = as
  , currentId = 0
  , maxAuctionTime = 99999999999999999999
  , maxConfigToStartTime = 99999999999999999
  , bidCurrency = bc
  , auctions = mempty
  , allowlist = ()
  , fee = feeData
  }

-- Contract
----------------------------------------------------------------------------

englishAuctionFA2FixedFeeContract
  :: T.Contract
      (ToT (AuctionFA2.AuctionEntrypoints NoAllowlist.Entrypoints))
      (ToT AuctionStorage)
englishAuctionFA2FixedFeeContract = $$(embedContractM (inBinFolder "english_auction_fa2_fixed_fee.tz"))
