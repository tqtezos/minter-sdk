-- | Tests on allowlisting capabilities.
module Test.Allowlisted
  ( OriginateAdminFn
  , allowlistUpdateAuthorizationChecks

  , AllowlistRestrictionCase (..)
  , AllowlistChecksSetup (..)
  , allowlistChecks
  ) where

import Test.Tasty (TestTree)

import Lorentz.Entrypoints
import Lorentz.Errors
import Lorentz.Value
import Morley.Nettest
import Morley.Nettest.Tasty (nettestScenarioCaps)

import Lorentz.Contracts.NonPausableSimpleAdmin (errNotAdmin)
import Test.Swaps.Util
import Test.Util

import qualified Indigo.Contracts.FA2Sample as FA2

-- | A function that originates a contract and also returns its admin.
type OriginateAdminFn param =
  forall m. (Monad m)
  => Address -> NettestT m (TAddress param)

allowlistUpdateAuthorizationChecks
  :: (ParameterContainsEntrypoints param
       '["Update_allowed" :> BigMap Address ()
        ]
     )
  => OriginateAdminFn param
  -> [TestTree]
allowlistUpdateAuthorizationChecks originateFromAdminFn =
  [ nettestScenarioCaps "Can be updated by admin" $ do
      (contract, swapAdmin) <- originateWithAdmin originateFromAdminFn

      withSender swapAdmin $
        call contract (Call @"Update_allowed") mempty

  , nettestScenarioCaps "Cannot be updated by non-admins" $ do
      (contract, _swapAdmin) <- originateWithAdmin originateFromAdminFn
      user <- newAddress "user"

      withSender user $
        call contract (Call @"Update_allowed") mempty
        & expectError contract errNotAdmin
  ]

-- | Single type of allowlist restriction, usually corresponds to one error.
data AllowlistRestrictionCase setup contractParam =
  forall err. (IsError err, Eq err) => AllowlistRestrictionCase
  { -- | Error raised when resitriction is violated.
    allowlistError :: err
    -- | Run an action that requires the specified FA2 to be allowlisted
    -- and fails otherwise.
  , allowlistRunRestrictedAction
      :: forall m. (Monad m)
      => setup -> TAddress contractParam -> TAddress FA2.FA2SampleParameter -> NettestT m ()
  }

-- | Setup for 'allowlistChecks'.
data AllowlistChecksSetup =
  forall contractParam setup.
    ( ParameterContainsEntrypoints contractParam '["Update_allowed" :> BigMap Address ()]
    ) =>
  AllowlistChecksSetup
  { -- | Run preparations, this includes the contract origination.
    allowlistCheckSetup
      :: forall m.
         (Monad m)
      => FA2Setup 1 1
      -> NettestT m (Address, TAddress contractParam, setup)

    -- | Restriction cases that needs to be tested.
    -- The cases should appear in the order in which the corresponding errors
    -- are checked in contract code.
  , allowlistRestrictionsCases :: NonEmpty (AllowlistRestrictionCase setup contractParam)

    -- | Addresses that must be always included into allowlist.
    -- In most cases should be just empty.
  , allowlistAlwaysIncluded :: setup -> [TAddress FA2.FA2SampleParameter]
  }

allowlistChecks :: AllowlistChecksSetup -> [TestTree]
allowlistChecks AllowlistChecksSetup{..} =
  let mkFullAllowlist setup addrs =
        mkAllowlistParam (addrs ++ allowlistAlwaysIncluded setup)
  in
  [ nettestScenarioCaps "Initially no FA2 works" $ do
      fa2Setup <- doFA2Setup
      (_admin, contract, setup) <- allowlistCheckSetup fa2Setup
      fa2 <- originateFA2 "fa2" fa2Setup [contract]

      case allowlistRestrictionsCases of
        AllowlistRestrictionCase{..} :| _ ->
          allowlistRunRestrictedAction setup contract fa2
            & expectError contract allowlistError

  , nettestScenarioCaps "Only FA2s that has been allowed work" $ do
      fa2Setup <- doFA2Setup
      (admin, contract, setup) <- allowlistCheckSetup fa2Setup
      fa2_1 <- originateFA2 "fa2-1" fa2Setup [contract]
      fa2_2 <- originateFA2 "fa2-2" fa2Setup [contract]

      withSender admin $
        call contract (Call @"Update_allowed") $ mkFullAllowlist setup [fa2_2]

      comment "Check that not yet allowed FA2 does not work"
      case allowlistRestrictionsCases of
        AllowlistRestrictionCase{..} :| _ -> do
          allowlistRunRestrictedAction setup contract fa2_1
            & expectError contract allowlistError

      comment "Allowed FA2 works"
      for_ allowlistRestrictionsCases $ \AllowlistRestrictionCase{..} ->
        allowlistRunRestrictedAction setup contract fa2_2

  , nettestScenarioCaps "Permission can be revoked" $ do
      fa2Setup <- doFA2Setup
      (admin, contract, setup) <- allowlistCheckSetup fa2Setup
      fa2 <- originateFA2 "fa2" fa2Setup [contract]
      fa2_another <- originateFA2 "fa2" fa2Setup [contract]

      withSender admin $
        call contract (Call @"Update_allowed") $ mkFullAllowlist setup [fa2]
      withSender admin $
        call contract (Call @"Update_allowed") $ mkFullAllowlist setup [fa2_another]

      case allowlistRestrictionsCases of
        AllowlistRestrictionCase{..} :| _ -> do
          allowlistRunRestrictedAction setup contract fa2
            & expectError contract allowlistError

  ]
