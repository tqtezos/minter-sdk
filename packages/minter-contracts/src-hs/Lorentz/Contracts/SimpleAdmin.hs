-- | Lorentz bindings for the non-pausable admin contract.
module Lorentz.Contracts.SimpleAdmin where

import Lorentz
import Fmt (Buildable(..), genericF)

-- Types
----------------------------------------------------------------------------

data AdminStorage = AdminStorage
  { admin :: Address
  , pendingAdmin :: Maybe Address
  , paused :: Bool
  }

customGeneric "AdminStorage" ligoLayout
deriving anyclass instance IsoValue AdminStorage
deriving anyclass instance HasAnnotation AdminStorage
instance Buildable AdminStorage where build = genericF

data AdminEntrypoints
  = Set_admin Address
  | Confirm_admin
  | Pause Bool

customGeneric "AdminEntrypoints" ligoLayout
deriving anyclass instance IsoValue AdminEntrypoints
deriving anyclass instance HasAnnotation AdminEntrypoints

instance ParameterHasEntrypoints AdminEntrypoints where
  type ParameterEntrypointsDerivation AdminEntrypoints = EpdPlain

initAdminStorage :: Address -> AdminStorage
initAdminStorage admin = AdminStorage
  { admin = admin
  , pendingAdmin = Nothing
  , paused = False 
  }

-- Errors
----------------------------------------------------------------------------

errNotAdmin :: MText
errNotAdmin = [mt|NOT_AN_ADMIN|]

errNotAdmin' :: MText -> MText
errNotAdmin' details = [mt|NOT_AN_ADMIN |] <> details

notPendingAdmin :: MText
notPendingAdmin = [mt|NOT_A_PENDING_ADMIN|]

noPendingAdmin :: MText
noPendingAdmin = [mt|NO_PENDING_ADMIN|]

errPaused :: MText 
errPaused = [mt|PAUSED|]
