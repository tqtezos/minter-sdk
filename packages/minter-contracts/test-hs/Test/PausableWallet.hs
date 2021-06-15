module Test.PausableWallet where

import Lorentz.Contracts.PausableWallet
import Lorentz.Contracts.PausableWalletContract
import Lorentz.Contracts.SimpleAdmin
import Lorentz.Value
import Morley.Nettest
import Morley.Nettest.Tasty (nettestScenarioCaps)
import Test.SimpleAdmin
import Test.Tasty (TestTree, testGroup)

test_AdminChecks :: TestTree
test_AdminChecks =
  adminOwnershipTransferChecks originatePausableWallet

test_PausableDefault :: TestTree
test_PausableDefault = testGroup "Send tez to default"
  [ nettestScenarioCaps "Tez can be sent to default when unpaused" $ do
    admin <- newAddress "admin"
    wallet <- originatePausableWallet admin
    call wallet CallDefault ()
  , nettestScenarioCaps "Tez cannot be sent to default when Paused" $ do
    admin <- newAddress "admin"
    wallet <- originatePausableWallet admin
    withSender admin $
      call wallet (Call @"Pause") True
    transfer TransferData
     { tdTo = wallet
     , tdAmount = toMutez 10
     , tdEntrypoint = DefEpName
     , tdParameter = ()
     }
      & expectError errPaused
  ]

test_SendAdminChecked :: TestTree
test_SendAdminChecked = testGroup "Send must be admin checked"
  [ nettestScenarioCaps "Send entrypoint is admin checked" $ do
    admin <- newAddress "admin"
    wallet <- originatePausableWallet admin
    call wallet CallDefault ()
    call wallet (Call @"Send") 10
      & expectError errNotAdmin
  ]

test_DefaultReceivesFee :: TestTree
test_DefaultReceivesFee  = testGroup "Default entrypoint receives fee sent"
  [ nettestScenarioCaps "Default entrypoint receives fee sent" $ do
    admin <- newAddress "admin"
    wallet <- originatePausableWallet admin
    initWalletBalance <- getBalance wallet
    transfer TransferData
     { tdTo = wallet
     , tdAmount = toMutez 10
     , tdEntrypoint = DefEpName
     , tdParameter = ()
     }
    finalWalletBalance <- getBalance wallet
    let amountReceived = finalWalletBalance - initWalletBalance
    amountReceived @== 10
  ]

test_SendsAmount :: TestTree
test_SendsAmount = testGroup "Send sends correct amount"
  [ nettestScenarioCaps "Send sends correct amount" $ do
    admin <- newAddress "admin"
    wallet <- originatePausableWallet admin
    transfer TransferData
     { tdTo = wallet
     , tdAmount = toMutez 10
     , tdEntrypoint = DefEpName
     , tdParameter = ()
     }
    initWalletBalance <- getBalance wallet
    withSender admin $
      call wallet (Call @"Send") 10
    finalWalletBalance <- getBalance wallet
    let amountSent = initWalletBalance - finalWalletBalance
    amountSent @== 10
  ]

originatePausableWallet
  :: MonadNettest caps base m
  => Address
  -> m (ContractHandler PausableWalletEntrypoints PausableWalletStorage)
originatePausableWallet admin = do
  originateSimple "pausable-wallet"
    (initPausableWalletStorage admin)
    pausableWalletContract
