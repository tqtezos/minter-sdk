{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Test.Marketplace.Util
  ( originateMarketplaceAllowlisted
  , originateMarketplaceAllowlistedToken
  , originateMarketplaceTezAllowlisted
  , originateMarketplaceTezAllowlistedToken
  ) where

import Lorentz.Value
import qualified Michelson.Typed as T
import Morley.Nettest

import Lorentz.Contracts.AllowlistSimple as AllowlistSimple
import Lorentz.Contracts.AllowlistToken as AllowlistToken
import Lorentz.Contracts.Marketplace.FA2
import Lorentz.Contracts.Marketplace.Tez
import qualified Lorentz.Contracts.PausableAdminOption as PausableAdminOption

originateMarketplaceAllowlisted
  :: MonadNettest caps base m
  => Address
  -> m (TAddress $ MarketplaceEntrypoints AllowlistSimple.Entrypoints)
originateMarketplaceAllowlisted admin = do
  originateSimple "marketplace"
    (initMarketplaceStorage (PausableAdminOption.initAdminStorage admin))
    marketplaceAllowlistedContract

originateMarketplaceAllowlistedToken
  :: MonadNettest caps base m
  => Address
  -> m (TAddress $ MarketplaceEntrypoints AllowlistToken.Entrypoints)
originateMarketplaceAllowlistedToken admin = do
  originateSimple "marketplace"
    (initMarketplaceStorage (PausableAdminOption.initAdminStorage admin))
    marketplaceAllowlistedTokenContract

originateMarketplaceTezAllowlisted
  :: MonadNettest caps base m
  => Address
  -> m (TAddress $ MarketplaceTezEntrypoints AllowlistSimple.Entrypoints)
originateMarketplaceTezAllowlisted admin = do
  originateSimple "marketplace-tez"
    (initMarketplaceTezStorage (PausableAdminOption.initAdminStorage admin))
    marketplaceTezAllowlistedContract

originateMarketplaceTezAllowlistedToken
  :: MonadNettest caps base m
  => Address
  -> m (TAddress $ MarketplaceTezEntrypoints AllowlistToken.Entrypoints)
originateMarketplaceTezAllowlistedToken admin = do
  originateSimple "marketplace-tez"
    (initMarketplaceTezStorage (PausableAdminOption.initAdminStorage admin))
    (T.convertContract marketplaceTezAllowlistedTokenContract)
