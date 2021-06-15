module Lorentz.Contracts.PausableWallet where

import Fmt (Buildable(..), genericF)
import Lorentz
import qualified Lorentz.Contracts.SimpleAdmin as SimpleAdmin

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
