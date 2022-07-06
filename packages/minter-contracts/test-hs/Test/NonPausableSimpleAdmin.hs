-- | Helpers to test simple admin functionality.
module Test.NonPausableSimpleAdmin
  ( OriginateAdminContractFn
  , adminOwnershipTransferChecks
  ) where

import Lorentz.Entrypoints
import Lorentz.Value
import Prelude hiding (swap)

import Test.Tasty (TestTree, testGroup)

import Morley.Nettest
import Morley.Nettest.Tasty (nettestScenarioCaps)

import Lorentz.Contracts.NonPausableSimpleAdmin

type OriginateAdminContractFn param storage =
  forall m. (Monad m)
  => Address -> NettestT m (ContractHandler param storage)

adminOwnershipTransferChecks
  :: ( ParameterContainsEntrypoints param
        [ "Set_admin" :> Address
        , "Confirm_admin" :> ()
        ]
     )
  => OriginateAdminContractFn param storage
  -> TestTree
adminOwnershipTransferChecks originateFn = testGroup "Admin functionality"
  [ nettestScenarioCaps "Ownership transfer works" $ do
      admin <- newAddress "admin"
      successor <- newAddress "successor"
      contract <- originateFn admin
      call contract (Call @"Set_admin") successor
        & withSender admin
      call contract (Call @"Confirm_admin") ()
        & withSender successor

      comment "Checking that ownership transfer really occured"
      call contract (Call @"Set_admin") admin
        & withSender successor

  , nettestScenarioCaps "Non-admin cannot set new admin" $ do
      admin <- newAddress "admin"
      successor <- newAddress "successor"
      contract <- originateFn admin
      call contract (Call @"Set_admin") successor
        & expectError errNotAdmin

  , nettestScenarioCaps "Cannot accept ownership before prior transfer of it" $ do
      admin <- newAddress "admin"
      contract <- originateFn admin

      call contract (Call @"Confirm_admin") ()
        & expectError noPendingAdmin

  , nettestScenarioCaps "Not pending admin cannot confirm ownership" $ do
      admin <- newAddress "admin"
      successor <- newAddress "successor"
      contract <- originateFn admin

      call contract (Call @"Set_admin") successor
        & withSender admin
      call contract (Call @"Confirm_admin") ()
        & expectError notPendingAdmin

  ]
