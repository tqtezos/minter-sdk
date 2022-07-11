module Lorentz.Contracts.MultiunitAuction.Contract
  ( auctionContract
  ) where

import Lorentz
import Lorentz.Test.Import (embedContractM)
import Lorentz.Contracts.MinterSdk (inBinFolder)
import Lorentz.Contracts.MultiunitAuction.Auction

auctionContract
  :: Contract
      AuctionEntrypoints
      AuctionStorage
auctionContract =
  $$(embedContractM (inBinFolder "multiunit_bonding_curve_auction_offchain_bid.tz"))
