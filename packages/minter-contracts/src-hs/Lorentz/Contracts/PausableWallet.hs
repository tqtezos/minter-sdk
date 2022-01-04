module Lorentz.Contracts.PausableWallet where

import Fmt (Buildable(..), genericF)
import Lorentz
import qualified Lorentz.Contracts.SimpleAdmin as SimpleAdmin
import Michelson.Test.Import (embedContractM)
import qualified Michelson.Typed as T
import Lorentz.Contracts.MinterSdk

-- Types
----------------------------------------------------------------------------
data PausableWalletStorage = PausableWalletStorage SimpleAdmin.AdminStorage

customGeneric "PausableWalletStorage" ligoCombLayout
deriving anyclass instance IsoValue PausableWalletStorage
deriving anyclass instance HasAnnotation PausableWalletStorage
instance Buildable PausableWalletStorage where build = genericF

data PausableWalletEntrypoints
  = Send Mutez 
  | Default 
  | Admin SimpleAdmin.AdminEntrypoints 

customGeneric "PausableWalletEntrypoints" ligoLayout
deriving anyclass instance IsoValue PausableWalletEntrypoints
deriving anyclass instance HasAnnotation PausableWalletEntrypoints

instance ParameterHasEntrypoints PausableWalletEntrypoints where
  type ParameterEntrypointsDerivation PausableWalletEntrypoints = EpdDelegate


initPausableWalletStorage :: Address -> PausableWalletStorage
initPausableWalletStorage = PausableWalletStorage . SimpleAdmin.initAdminStorage 

pausableWalletContract
  :: T.Contract
      (ToT PausableWalletEntrypoints)
      (ToT PausableWalletStorage)
pausableWalletContract =
  $$(embedContractM (inBinFolder "pausable_wallet.tz"))