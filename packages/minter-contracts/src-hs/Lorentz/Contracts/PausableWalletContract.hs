module Lorentz.Contracts.PausableWalletContract where

import Lorentz
import Lorentz.Contracts.MinterSdk
import Lorentz.Test.Import (embedContractM)

import Lorentz.Contracts.PausableWallet

pausableWalletContract
  :: Contract PausableWalletEntrypoints PausableWalletStorage
pausableWalletContract =
  $$(embedContractM (inBinFolder "pausable_wallet.tz"))
