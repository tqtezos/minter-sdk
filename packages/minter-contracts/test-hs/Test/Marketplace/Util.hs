{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Test.Marketplace.Util
  ( originateMarketplaceAllowlisted
  , originateMarketplaceTezAllowlisted
  ) where

import Lorentz.Value
import qualified Michelson.Typed as T
import Morley.Nettest

import Lorentz.Contracts.AllowlistSimple as AllowlistSimple
import Lorentz.Contracts.Marketplace.FA2
import Lorentz.Contracts.Marketplace.Tez
import qualified Lorentz.Contracts.PausableAdminOption as PausableAdminOption

originateMarketplaceAllowlisted
  :: MonadNettest caps base m
  => Address
  -> m (TAddress $ MarketplaceEntrypoints AllowlistSimple.Allowlist)
originateMarketplaceAllowlisted admin = do
  TAddress <$> originateUntypedSimple "marketplace"
    (T.untypeValue $ T.toVal $
      initMarketplaceStorage @AllowlistSimple.Allowlist
        (PausableAdminOption.initAdminStorage admin))
    (T.convertContract marketplaceAllowlistedContract)

originateMarketplaceTezAllowlisted
  :: MonadNettest caps base m
  => Address
  -> m (TAddress $ MarketplaceTezEntrypoints AllowlistSimple.Allowlist)
originateMarketplaceTezAllowlisted admin = do
  TAddress <$> originateUntypedSimple "marketplace-tez"
    (T.untypeValue $ T.toVal $
      initMarketplaceTezStorage @AllowlistSimple.Allowlist
        (PausableAdminOption.initAdminStorage admin))
    (T.convertContract marketplaceTezAllowlistedContract)
