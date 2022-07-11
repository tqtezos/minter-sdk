module Test.MultiunitAuction.Util where
import Lorentz.Value
import qualified Michelson.Typed as T
import Morley.Nettest

import qualified Lorentz.Contracts.MultiunitAuction.Auction as Auction
import qualified Lorentz.Contracts.MultiunitAuction.Contract as AuctionContract
import qualified Lorentz.Contracts.PausableAdminOption as PausableAdminOption 

originateAuction
  :: MonadNettest caps base m
  => Address
  -> m $ ContractHandler
       Auction.AuctionEntrypoints
       Auction.AuctionStorage
originateAuction admin = do
  originateSimple "multiunit-auction"
    (Auction.initAuctionStorage (PausableAdminOption.initAdminStorage admin))
    AuctionContract.auctionContract