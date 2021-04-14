{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Test.Marketplace.Util
  ( originateMarketplaceAllowlisted
  , originateMarketplaceTezAllowlisted
  ) where

import Lorentz.Value
import qualified Michelson.Typed as T
import Morley.Nettest

import Lorentz.Contracts.Marketplace.Allowlisted
import Lorentz.Contracts.Marketplace.FA2
import Lorentz.Contracts.Marketplace.Tez
import qualified Lorentz.Contracts.PausableAdminOption as PausableAdminOption

originateMarketplaceAllowlisted
  :: MonadNettest caps base m
  => Address
  -> m (TAddress $ AllowlistedEntrypoints MarketplaceEntrypoints)
originateMarketplaceAllowlisted admin = do
  TAddress <$> originateUntypedSimple "marketplace"
    (T.untypeValue $ T.toVal $
      initAllowlistedStorage $
      initMarketplaceStorage (PausableAdminOption.initAdminStorage admin))
    (T.convertContract marketplaceAllowlistedContract)

originateMarketplaceTezAllowlisted
  :: MonadNettest caps base m
  => Address
  -> m (TAddress $ AllowlistedEntrypoints MarketplaceTezEntrypoints)
originateMarketplaceTezAllowlisted admin = do
  TAddress <$> originateUntypedSimple "marketplace-tez"
    (T.untypeValue $ T.toVal $
      initAllowlistedStorage $
      initMarketplaceTezStorage (PausableAdminOption.initAdminStorage admin))
    (T.convertContract marketplaceTezAllowlistedContract)
