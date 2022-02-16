{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Test.EnglishAuction.Util
  ( originateAuctionFA2Allowlisted
  , originateAuctionFA2AllowlistedToken
  , originateAuctionTezAllowlisted
  , originateAuctionTezAllowlistedToken
  , originateAuctionTezPermitAllowlisted
  , originateAuctionTezPermitAllowlistedToken
  , originateAuctionTezOffchainConsolation
  , originateAuctionTezOffchainPositional
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
import qualified Lorentz.Contracts.EnglishAuction.ConsolationOffchain as ConsolationOffchain
import qualified Lorentz.Contracts.NoAllowlist as NoAllowlist

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

originateAuctionFA2AllowlistedToken
  :: MonadNettest caps base m
  => AuctionFA2.BidCurrency
  -> Address
  -> m (TAddress $ AuctionFA2.AuctionEntrypoints AllowlistToken.Entrypoints)
originateAuctionFA2AllowlistedToken bidCurrency admin = do
  TAddress <$> originateUntypedSimple "auction-fa2"
    (T.untypeValue $ T.toVal $
      AuctionFA2.initAuctionStorage @AllowlistToken.Allowlist
        (PausableAdminOption.initAdminStorage admin)
        bidCurrency
    )
    (T.convertContract AuctionFA2.auctionFA2AllowlistedTokenContract)

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

originateAuctionTezOffchainConsolation
  :: MonadNettest caps base m
  => Address
  -> m (TAddress $ ConsolationOffchain.AuctionEntrypoints NoAllowlist.Entrypoints)
originateAuctionTezOffchainConsolation admin = do
  TAddress <$> originateUntypedSimple "auction-tez"
    (T.untypeValue $ T.toVal $
      ConsolationOffchain.initAuctionStorage @NoAllowlist.Allowlist
        (PausableAdminOption.initAdminStorage admin))
    (T.convertContract ConsolationOffchain.consolationAuctionTezContract)

originateAuctionTezOffchainPositional
  :: MonadNettest caps base m
  => Address
  -> m (TAddress $ ConsolationOffchain.AuctionEntrypoints NoAllowlist.Entrypoints)
originateAuctionTezOffchainPositional admin = do
  TAddress <$> originateUntypedSimple "auction-tez"
    (T.untypeValue $ T.toVal $
      ConsolationOffchain.initAuctionStorage @NoAllowlist.Allowlist
        (PausableAdminOption.initAdminStorage admin))
    (T.convertContract ConsolationOffchain.positionalAuctionTezContract)

originateAuctionTezAllowlistedToken
  :: MonadNettest caps base m
  => Address
  -> m (TAddress $ AuctionTez.AuctionEntrypoints AllowlistToken.Entrypoints)
originateAuctionTezAllowlistedToken admin = do
  TAddress <$> originateUntypedSimple "auction-tez"
    (T.untypeValue $ T.toVal $
      AuctionTez.initAuctionStorage @AllowlistToken.Allowlist
        (PausableAdminOption.initAdminStorage admin))
    (T.convertContract AuctionTez.auctionTezAllowlistedTokenContract)

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

originateAuctionTezPermitAllowlistedToken
  :: MonadNettest caps base m
  => Address
  -> m (TAddress $ AuctionPermitTez.PermitAuctionEntrypoints AllowlistToken.Entrypoints)
originateAuctionTezPermitAllowlistedToken admin = do
  TAddress <$> originateUntypedSimple "auction-permit-tez"
    (T.untypeValue $ T.toVal $
      AuctionPermitTez.initPermitAuctionStorage @AllowlistToken.Allowlist
        (PausableAdminOption.initAdminStorage admin))
    (T.convertContract AuctionPermitTez.auctionTezPermitAllowlistedTokenContract)
