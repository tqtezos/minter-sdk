{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Test.EnglishAuction.Util
  ( originateAuctionFA2Allowlisted
  , originateAuctionFA2AllowlistedToken
  , originateAuctionTezAllowlisted
  , originateAuctionTezAllowlistedToken
  , originateAuctionTezPermitAllowlisted
  , originateAuctionTezPermitAllowlistedToken
  ) where

import Lorentz.Value
import qualified Michelson.Typed as T
import Morley.Nettest

import Lorentz.Contracts.AllowlistSimple as AllowlistSimple
import Lorentz.Contracts.AllowlistToken as AllowlistToken
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
  originateSimple "auction-fa2"
    (AuctionFA2.initAuctionStorage
      (PausableAdminOption.initAdminStorage admin)
      bidCurrency
    )
    AuctionFA2.auctionFA2AllowlistedContract

originateAuctionFA2AllowlistedToken
  :: MonadNettest caps base m
  => AuctionFA2.BidCurrency
  -> Address
  -> m (TAddress $ AuctionFA2.AuctionEntrypoints AllowlistToken.Entrypoints)
originateAuctionFA2AllowlistedToken bidCurrency admin = do
  originateSimple "auction-fa2"
    (AuctionFA2.initAuctionStorage
      (PausableAdminOption.initAdminStorage admin)
      bidCurrency
    )
    AuctionFA2.auctionFA2AllowlistedTokenContract

originateAuctionTezAllowlisted
  :: MonadNettest caps base m
  => Address
  -> m (TAddress $ AuctionTez.AuctionEntrypoints AllowlistSimple.Entrypoints)
originateAuctionTezAllowlisted admin = do
  originateSimple "auction-tez"
    (AuctionTez.initAuctionStorage (PausableAdminOption.initAdminStorage admin))
    AuctionTez.auctionTezAllowlistedContract

originateAuctionTezAllowlistedToken
  :: MonadNettest caps base m
  => Address
  -> m (TAddress $ AuctionTez.AuctionEntrypoints AllowlistToken.Entrypoints)
originateAuctionTezAllowlistedToken admin = do
  originateSimple "auction-tez"
    (AuctionTez.initAuctionStorage (PausableAdminOption.initAdminStorage admin))
    AuctionTez.auctionTezAllowlistedTokenContract

originateAuctionTezPermitAllowlisted
  :: MonadNettest caps base m
  => Address
  -> m (TAddress $ AuctionPermitTez.PermitAuctionEntrypoints AllowlistSimple.Entrypoints)
originateAuctionTezPermitAllowlisted admin = do
  originateSimple "auction-permit-tez"
    (AuctionPermitTez.initPermitAuctionStorage
       (PausableAdminOption.initAdminStorage admin))
    AuctionPermitTez.auctionTezPermitAllowlistedContract

originateAuctionTezPermitAllowlistedToken
  :: MonadNettest caps base m
  => Address
  -> m (TAddress $ AuctionPermitTez.PermitAuctionEntrypoints AllowlistToken.Entrypoints)
originateAuctionTezPermitAllowlistedToken admin = do
  originateSimple "auction-permit-tez"
    (AuctionPermitTez.initPermitAuctionStorage
      (PausableAdminOption.initAdminStorage admin))
    AuctionPermitTez.auctionTezPermitAllowlistedTokenContract
