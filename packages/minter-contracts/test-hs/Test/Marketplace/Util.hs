{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Test.Marketplace.Util
  ( originateMarketplaceAllowlisted
  , originateMarketplaceAllowlistedToken
  , originateMarketplaceTezAllowlisted
  , originateMarketplaceTezAllowlistedToken
  , originateOffchainTezMarketplace
  ) where

import Lorentz.Value
import qualified Michelson.Typed as T
import Morley.Nettest

import Lorentz.Contracts.AllowlistSimple as AllowlistSimple
import Lorentz.Contracts.AllowlistToken as AllowlistToken
import Lorentz.Contracts.Marketplace.FA2
import Lorentz.Contracts.Marketplace.Tez
import Lorentz.Contracts.Marketplace.TezOffchain
import qualified Lorentz.Contracts.PausableAdminOption as PausableAdminOption

originateMarketplaceAllowlisted
  :: MonadNettest caps base m
  => Address
  -> m (TAddress $ MarketplaceEntrypoints AllowlistSimple.Entrypoints)
originateMarketplaceAllowlisted admin = do
  TAddress <$> originateUntypedSimple "marketplace"
    (T.untypeValue $ T.toVal $
      initMarketplaceStorage @AllowlistSimple.Allowlist
        (PausableAdminOption.initAdminStorage admin))
    (T.convertContract marketplaceAllowlistedContract)

originateMarketplaceAllowlistedToken
  :: MonadNettest caps base m
  => Address
  -> m (TAddress $ MarketplaceEntrypoints AllowlistToken.Entrypoints)
originateMarketplaceAllowlistedToken admin = do
  TAddress <$> originateUntypedSimple "marketplace"
    (T.untypeValue $ T.toVal $
      initMarketplaceStorage @AllowlistToken.Allowlist
        (PausableAdminOption.initAdminStorage admin))
    (T.convertContract marketplaceAllowlistedTokenContract)

originateMarketplaceTezAllowlisted
  :: MonadNettest caps base m
  => Address
  -> m (TAddress $ MarketplaceTezEntrypoints AllowlistSimple.Entrypoints)
originateMarketplaceTezAllowlisted admin = do
  TAddress <$> originateUntypedSimple "marketplace-tez"
    (T.untypeValue $ T.toVal $
      initMarketplaceTezStorage @AllowlistSimple.Allowlist
        (PausableAdminOption.initAdminStorage admin))
    (T.convertContract marketplaceTezAllowlistedContract)

originateMarketplaceTezAllowlistedToken
  :: MonadNettest caps base m
  => Address
  -> m (TAddress $ MarketplaceTezEntrypoints AllowlistToken.Entrypoints)
originateMarketplaceTezAllowlistedToken admin = do
  TAddress <$> originateUntypedSimple "marketplace-tez"
    (T.untypeValue $ T.toVal $
      initMarketplaceTezStorage @AllowlistToken.Allowlist
        (PausableAdminOption.initAdminStorage admin))
    (T.convertContract marketplaceTezAllowlistedTokenContract)

originateOffchainTezMarketplace
  :: MonadNettest caps base m
  => Address
  -> m (TAddress $ MarketplaceTezOffchainEntrypoints AllowlistSimple.Entrypoints)
originateOffchainTezMarketplace admin = do
  TAddress <$> originateUntypedSimple "marketplace"
    (T.untypeValue $ T.toVal $
      initMarketplaceTezOffchainStorage @AllowlistSimple.Allowlist
        (PausableAdminOption.initAdminStorage admin))
    (T.convertContract marketplaceTezOffchainContract)