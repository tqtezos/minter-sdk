-- | Tests on allowlisting capabilities.
module Test.Allowlisted
  ( OriginateAdminFn
  , allowlistUpdateAuthorizationChecks

  , AllowlistRestrictionCase (..)
  , AllowlistChecksSetup (..)
  , allowlistSimpleChecks
  , allowlistTokenChecks
  ) where

import Test.Tasty (TestTree, testGroup)

import Lorentz.Constraints
import Lorentz.Entrypoints
import Lorentz.Value
import Morley.Nettest
import Morley.Nettest.Tasty (nettestScenarioCaps)

import qualified Lorentz.Contracts.AllowlistSimple as AllowlistSimple
import qualified Lorentz.Contracts.AllowlistToken as AllowlistToken
import Lorentz.Contracts.NonPausableSimpleAdmin (errNotAdmin)
import Lorentz.Contracts.Spec.FA2Interface (TokenId)
import Test.Swaps.Util
import Test.Util

import qualified Indigo.Contracts.FA2Sample as FA2

-- | A function that originates a contract and also returns its admin.
type OriginateAdminFn param storage =
  forall m. (Monad m)
  => Address -> NettestT m (ContractHandler param storage)

allowlistUpdateAuthorizationChecks
  :: (ParameterContainsEntrypoints param
       '["Update_allowed" :> allowlistUpdateParam
        ]
     , KnownValue allowlistUpdateParam, Monoid allowlistUpdateParam
     )
  => OriginateAdminFn param storage
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
        & expectFailedWith errNotAdmin
  ]

-- | Single type of allowlist restriction, usually corresponds to one error.
data AllowlistRestrictionCase setup contractParam =
  forall err. (NiceConstant err, Eq err) => AllowlistRestrictionCase
  { -- | Error raised when restriction is violated.
    allowlistError :: err
    -- | Run an action that requires the specified FA2 to be allowlisted
    -- and fails otherwise.
  , allowlistRunRestrictedAction
      :: forall contractStorage m. (Monad m)
      => setup
      -> ContractHandler contractParam contractStorage
      -> (ContractHandler FA2.FA2SampleParameter FA2.Storage, TokenId)
      -> NettestT m ()
  }

-- | Setup for 'simpleAllowlistChecks'.
data AllowlistChecksSetup eps =
  forall contractParam contractStorage setup.
    ( ParameterContainsEntrypoints contractParam eps
    ) =>
  AllowlistChecksSetup
  { -- | Run preparations, this includes the contract origination.
    allowlistCheckSetup
      :: forall m.
         (Monad m)
      => FA2Setup 1 2
      -> NettestT m (Address, ContractHandler contractParam contractStorage, setup)

    -- | Restriction cases that needs to be tested.
    -- The cases should appear in the order in which the corresponding errors
    -- are checked in contract code.
  , allowlistRestrictionsCases :: NonEmpty (AllowlistRestrictionCase setup contractParam)

    -- | Addresses and tokens that must be always included into allowlist.
    -- In most cases should be just empty.
  , allowlistAlwaysIncluded
      :: setup
      -> ([(ContractHandler FA2.FA2SampleParameter FA2.Storage, TokenId)])
  }

allowlistSimpleChecks
  :: AllowlistChecksSetup
      '["Update_allowed" :> AllowlistSimple.Entrypoints]
  -> [TestTree]
allowlistSimpleChecks AllowlistChecksSetup{..} =
  let mkFullAllowlist setup addrs =
        mkAllowlistSimpleParam (addrs ++ map fst (allowlistAlwaysIncluded setup))
  in
  [ nettestScenarioCaps "Initially no FA2 works" $ do
      fa2Setup <- doFA2Setup
      let tokenId ::< _ = sTokens fa2Setup
      (_admin, contract, setup) <- allowlistCheckSetup fa2Setup
      fa2 <- originateFA2 "fa2" fa2Setup [contract]

      case allowlistRestrictionsCases of
        AllowlistRestrictionCase{..} :| _ ->
          allowlistRunRestrictedAction setup contract (fa2, tokenId)
            & expectFailedWith allowlistError

  , nettestScenarioCaps "Only FA2s that has been allowed work" $ do
      fa2Setup <- doFA2Setup
      let tokenId ::< _ = sTokens fa2Setup
      (admin, contract, setup) <- allowlistCheckSetup fa2Setup
      fa2_1 <- originateFA2 "fa2-1" fa2Setup [contract]
      fa2_2 <- originateFA2 "fa2-2" fa2Setup [contract]

      withSender admin $
        call contract (Call @"Update_allowed") $ mkFullAllowlist setup [fa2_2]

      comment "Check that not yet allowed FA2 does not work"
      for_ @_ @_ @() allowlistRestrictionsCases \AllowlistRestrictionCase{..} -> do
        allowlistRunRestrictedAction setup contract (fa2_1, tokenId)
          & expectFailedWith allowlistError

      comment "Allowed FA2 works"
      for_ @_ @_ @() allowlistRestrictionsCases $ \AllowlistRestrictionCase{..} ->
        allowlistRunRestrictedAction setup contract (fa2_2, tokenId)

  , nettestScenarioCaps "Permission can be revoked" $ do
      fa2Setup <- doFA2Setup
      let tokenId ::< _ = sTokens fa2Setup
      (admin, contract, setup) <- allowlistCheckSetup fa2Setup
      fa2 <- originateFA2 "fa2" fa2Setup [contract]
      fa2_another <- originateFA2 "fa2" fa2Setup [contract]

      withSender admin $
        call contract (Call @"Update_allowed") $ mkFullAllowlist setup [fa2]
      withSender admin $
        call contract (Call @"Update_allowed") $ mkFullAllowlist setup [fa2_another]

      case allowlistRestrictionsCases of
        AllowlistRestrictionCase{..} :| _ -> do
          allowlistRunRestrictedAction setup contract (fa2, tokenId)
            & expectFailedWith allowlistError

  ]

allowlistTokenChecks
  :: AllowlistChecksSetup
      '["Update_allowed" :> AllowlistToken.Entrypoints]
  -> [TestTree]
allowlistTokenChecks AllowlistChecksSetup{..} =
  let addAlwaysIncluded setup = foldMap
        (\(fa2, tokenId) -> mempty
            { AllowlistToken.toAdd = one (toAddress fa2, AllowlistToken.TokenIdsAllowed (one tokenId)) }
        )
        (allowlistAlwaysIncluded setup)
  in
  [ testGroup "FA2 address"

    [ nettestScenarioCaps "Initially no FA2 works" $ do
        fa2Setup <- doFA2Setup
        let tokenId ::< _ = sTokens fa2Setup
        (_admin, contract, setup) <- allowlistCheckSetup fa2Setup
        fa2 <- originateFA2 "fa2" fa2Setup [contract]

        case allowlistRestrictionsCases of
          AllowlistRestrictionCase{..} :| _ ->
            allowlistRunRestrictedAction setup contract (fa2, tokenId)
              & expectFailedWith allowlistError

    , nettestScenarioCaps "Only FA2s that has been allowed work" $ do
        fa2Setup <- doFA2Setup
        let tokenId ::< _ = sTokens fa2Setup
        (admin, contract, setup) <- allowlistCheckSetup fa2Setup
        fa2_1 <- originateFA2 "fa2-1" fa2Setup [contract]
        fa2_2 <- originateFA2 "fa2-2" fa2Setup [contract]

        withSender admin $
          call contract (Call @"Update_allowed") $
            addAlwaysIncluded setup <>
            mempty
              { AllowlistToken.toAdd = one (toAddress fa2_2, AllowlistToken.AllTokenIdsAllowed) }

        comment "Check that not yet allowed FA2 does not work"
        () <- case allowlistRestrictionsCases of
          AllowlistRestrictionCase{..} :| _ -> do
            allowlistRunRestrictedAction setup contract (fa2_1, tokenId)
              & expectFailedWith allowlistError

        comment "Allowed FA2 works"
        for_ @_ @_ @() allowlistRestrictionsCases \AllowlistRestrictionCase{..} ->
          allowlistRunRestrictedAction setup contract (fa2_2, tokenId)

    , nettestScenarioCaps "Permission can be revoked" $ do
        fa2Setup <- doFA2Setup
        let tokenId ::< _ = sTokens fa2Setup
        (admin, contract, setup) <- allowlistCheckSetup fa2Setup
        fa2 <- originateFA2 "fa2" fa2Setup [contract]
        fa2_another <- originateFA2 "fa2" fa2Setup [contract]

        withSender admin $
          call contract (Call @"Update_allowed") $
            addAlwaysIncluded setup <>
            mempty
              { AllowlistToken.toAdd = one (toAddress fa2, AllowlistToken.AllTokenIdsAllowed) }
        withSender admin $
          call contract (Call @"Update_allowed") $
            mempty
              { AllowlistToken.toAdd = one (toAddress fa2_another, AllowlistToken.AllTokenIdsAllowed)
              , AllowlistToken.toRemove = one (toAddress fa2)
              }

        case allowlistRestrictionsCases of
          AllowlistRestrictionCase{..} :| _ -> do
            allowlistRunRestrictedAction setup contract (fa2, tokenId)
              & expectFailedWith allowlistError

    , nettestScenarioCaps "Can safely remove non-existing address" $ do
        fa2Setup <- doFA2Setup
        (admin, contract, _setup) <- allowlistCheckSetup fa2Setup
        fa2 <- originateFA2 "fa2" fa2Setup [contract]

        withSender admin $ do
          call contract (Call @"Update_allowed") $
            mempty
              { AllowlistToken.toRemove = one (toAddress fa2)
              }

    , nettestScenarioCaps "Removal preceeds addition" $ do
        fa2Setup <- doFA2Setup
        let tokenId ::< _ = sTokens fa2Setup
        (admin, contract, setup) <- allowlistCheckSetup fa2Setup
        fa2 <- originateFA2 "fa2" fa2Setup [contract]

        withSender admin $ do
          call contract (Call @"Update_allowed") $
            addAlwaysIncluded setup <>
            mempty
              { AllowlistToken.toRemove =
                  one (toAddress fa2)
              ,  AllowlistToken.toAdd =
                  one (toAddress fa2, AllowlistToken.AllTokenIdsAllowed)
              }

        for_ @_ @_ @() allowlistRestrictionsCases \AllowlistRestrictionCase{..} ->
          allowlistRunRestrictedAction setup contract (fa2, tokenId)

    ]

  , testGroup "TokenIds"

    [ nettestScenarioCaps "Only tokenIds that have been allowed work" $ do
        fa2Setup <- doFA2Setup
        let tokenId1 ::< tokenId2 ::< SNil = sTokens fa2Setup
        (admin, contract, setup) <- allowlistCheckSetup fa2Setup
        fa2 <- originateFA2 "fa2" fa2Setup [contract]

        withSender admin $
          call contract (Call @"Update_allowed") $
            addAlwaysIncluded setup <>
            mempty
              { AllowlistToken.toAdd =
                  one (toAddress fa2, AllowlistToken.TokenIdsAllowed (one tokenId1))
              }

        comment "Check that not yet allowed tokenId does not work"
        () <- case allowlistRestrictionsCases of
          AllowlistRestrictionCase{..} :| _ -> do
            allowlistRunRestrictedAction setup contract (fa2, tokenId2)
              & expectFailedWith allowlistError

        comment "Allowed tokenId works"
        for_ @_ @_ @() allowlistRestrictionsCases \AllowlistRestrictionCase{..} ->
          allowlistRunRestrictedAction setup contract (fa2, tokenId1)

    , nettestScenarioCaps "TokenIds are permitted per FA2 address" $ do
        fa2Setup <- doFA2Setup
        let tokenId1 ::< tokenId2 ::< SNil = sTokens fa2Setup
        (admin, contract, setup) <- allowlistCheckSetup fa2Setup
        fa2_1 <- originateFA2 "fa2-1" fa2Setup [contract]
        fa2_2 <- originateFA2 "fa2-2" fa2Setup [contract]

        comment "Checking that allowing tokenId2 for fa2_1 does not affect fa2_2"
        withSender admin $ do
          call contract (Call @"Update_allowed") $
            addAlwaysIncluded setup <>
            mempty
              { AllowlistToken.toAdd = mconcat
                  [ one (toAddress fa2_1, AllowlistToken.TokenIdsAllowed (one tokenId1 <> one tokenId2))
                  , one (toAddress fa2_2, AllowlistToken.TokenIdsAllowed (one tokenId1))
                  ]
              }

        for_ @_ @_ @() allowlistRestrictionsCases \AllowlistRestrictionCase{..} ->
          allowlistRunRestrictedAction setup contract (fa2_2, tokenId2)
              & expectFailedWith allowlistError

    , nettestScenarioCaps "Revoked tokenIds do not work" $ do
        fa2Setup <- doFA2Setup
        let tokenId1 ::< tokenId2 ::< SNil = sTokens fa2Setup
        (admin, contract, setup) <- allowlistCheckSetup fa2Setup
        fa2 <- originateFA2 "fa2" fa2Setup [contract]

        withSender admin $ do
          call contract (Call @"Update_allowed") $
            addAlwaysIncluded setup <>
            mempty
              { AllowlistToken.toAdd =
                  one (toAddress fa2, AllowlistToken.TokenIdsAllowed (one tokenId1 <> one tokenId2))
              }
          call contract (Call @"Update_allowed") $
            mempty
              { AllowlistToken.toAdd =
                  one (toAddress fa2, AllowlistToken.TokenIdsAllowed (one tokenId1))
              }

        for_ @_ @_ @() allowlistRestrictionsCases \AllowlistRestrictionCase{..} ->
          allowlistRunRestrictedAction setup contract (fa2, tokenId2)
              & expectFailedWith allowlistError

    ]

  ]
