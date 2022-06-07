module Test.MultiunitAuction.Util where
import Lorentz.Value
import qualified Michelson.Typed as T
import Morley.Nettest

import qualified Lorentz.Contracts.MultiunitAuction.Auction as Auction
import qualified Lorentz.Contracts.PausableAdminOption as PausableAdminOption 

originateAuction
  :: MonadNettest caps base m
  => Address
  -> m (TAddress $ Auction.AuctionEntrypoints)
originateAuction admin = do
  TAddress <$> originateUntypedSimple "multiunit-auction"
    (T.untypeValue $ T.toVal $
      Auction.initAuctionStorage 
        (PausableAdminOption.initAdminStorage admin))
    (T.convertContract Auction.auctionContract)