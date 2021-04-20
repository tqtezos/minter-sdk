{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Test.EnglishAuction.Util
  ( originateAuctionFA2Allowlisted
  , originateAuctionTezAllowlisted
  , originateAuctionTezPermitAllowlisted
  ) where

import Lorentz.Value
import qualified Michelson.Typed as T
import Morley.Nettest

import Lorentz.Contracts.AllowlistSimple as AllowlistSimple
import qualified Lorentz.Contracts.EnglishAuction.FA2 as AuctionFA2
import qualified Lorentz.Contracts.EnglishAuction.Tez as AuctionTez
import qualified Lorentz.Contracts.EnglishAuction.TezPermit as AuctionPermitTez
import qualified Lorentz.Contracts.PausableAdminOption as PausableAdminOption

originateAuctionFA2Allowlisted
  :: MonadNettest caps base m
  => AuctionFA2.BidCurrency
  -> Address
  -> m (TAddress $ AuctionFA2.AuctionEntrypoints AllowlistSimple.Entrypoints)
originateAuctionFA2Allowlisted bidCurrency admin = do
  TAddress <$> originateUntypedSimple "auction-fa2"
    (T.untypeValue $ T.toVal $
      AuctionFA2.initAuctionStorage @AllowlistSimple.Allowlist
        (PausableAdminOption.initAdminStorage admin)
        bidCurrency
    )
    (T.convertContract AuctionFA2.auctionFA2AllowlistedContract)

originateAuctionTezAllowlisted
  :: MonadNettest caps base m
  => Address
  -> m (TAddress $ AuctionTez.AuctionEntrypoints AllowlistSimple.Entrypoints)
originateAuctionTezAllowlisted admin = do
  TAddress <$> originateUntypedSimple "auction-tez"
    (T.untypeValue $ T.toVal $
      AuctionTez.initAuctionStorage @AllowlistSimple.Allowlist
        (PausableAdminOption.initAdminStorage admin))
    (T.convertContract AuctionTez.auctionTezAllowlistedContract)

originateAuctionTezPermitAllowlisted
  :: MonadNettest caps base m
  => Address
  -> m (TAddress $ AuctionPermitTez.PermitAuctionEntrypoints AllowlistSimple.Entrypoints)
originateAuctionTezPermitAllowlisted admin = do
  TAddress <$> originateUntypedSimple "auction-permit-tez"
    (T.untypeValue $ T.toVal $
      AuctionPermitTez.initPermitAuctionStorage @AllowlistSimple.Allowlist
        (PausableAdminOption.initAdminStorage admin))
    (T.convertContract AuctionPermitTez.auctionTezPermitAllowlistedContract)
