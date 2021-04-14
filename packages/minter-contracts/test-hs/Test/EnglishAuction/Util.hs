{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Test.EnglishAuction.Util
  ( originateAuctionFA2Allowlisted
  , originateAuctionTezAllowlisted
  , originateAuctionTezPermitAllowlisted
  ) where

import Lorentz.Value
import qualified Michelson.Typed as T
import Morley.Nettest

import Lorentz.Contracts.EnglishAuction.Allowlisted
import qualified Lorentz.Contracts.EnglishAuction.FA2 as AuctionFA2
import qualified Lorentz.Contracts.EnglishAuction.Tez as AuctionTez
import qualified Lorentz.Contracts.EnglishAuction.TezPermit as AuctionPermitTez
import qualified Lorentz.Contracts.PausableAdminOption as PausableAdminOption

originateAuctionFA2Allowlisted
  :: MonadNettest caps base m
  => AuctionFA2.BidCurrency
  -> Address
  -> m (TAddress $ AllowlistedEntrypoints AuctionFA2.AuctionEntrypoints)
originateAuctionFA2Allowlisted bidCurrency admin = do
  TAddress <$> originateUntypedSimple "auction-fa2"
    (T.untypeValue $ T.toVal $
      initAllowlistedStorage $
      AuctionFA2.initAuctionStorage
        (PausableAdminOption.initAdminStorage admin)
        bidCurrency
    )
    (T.convertContract auctionFA2AllowlistedContract)

originateAuctionTezAllowlisted
  :: MonadNettest caps base m
  => Address
  -> m (TAddress $ AllowlistedEntrypoints AuctionTez.AuctionEntrypoints)
originateAuctionTezAllowlisted admin = do
  TAddress <$> originateUntypedSimple "auction-tez"
    (T.untypeValue $ T.toVal $
      initAllowlistedStorage $
      AuctionTez.initAuctionStorage (PausableAdminOption.initAdminStorage admin))
    (T.convertContract auctionTezAllowlistedContract)

originateAuctionTezPermitAllowlisted
  :: MonadNettest caps base m
  => Address
  -> m (TAddress $ AllowlistedEntrypoints AuctionPermitTez.PermitAuctionEntrypoints)
originateAuctionTezPermitAllowlisted admin = do
  TAddress <$> originateUntypedSimple "auction-permit-tez"
    (T.untypeValue $ T.toVal $
      initAllowlistedStorage $
      AuctionPermitTez.initPermitAuctionStorage (PausableAdminOption.initAdminStorage admin))
    (T.convertContract auctionTezPermitAllowlistedContract)
