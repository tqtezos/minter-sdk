-- | Lorentz bindings for the pausable admin contract.
module Lorentz.Contracts.PausableAdminOption where

import Fmt (Buildable(..), genericF)
import Lorentz

-- Types
----------------------------------------------------------------------------

data AdminStorageRecord = AdminStorage
  { admin :: Address
  , pendingAdmin :: Maybe Address
  , paused :: Bool
  }
  deriving stock (Show, Eq)

customGeneric "AdminStorageRecord" ligoLayout
deriving anyclass instance IsoValue AdminStorageRecord
deriving anyclass instance HasAnnotation AdminStorageRecord
instance Buildable AdminStorageRecord where build = genericF

type AdminStorage = Maybe AdminStorageRecord

data AdminEntrypoints
  = Set_admin Address
  | Confirm_admin
  | Pause Bool
  deriving stock (Show, Eq)

customGeneric "AdminEntrypoints" ligoLayout
deriving anyclass instance IsoValue AdminEntrypoints
deriving anyclass instance HasAnnotation AdminEntrypoints

instance ParameterHasEntrypoints AdminEntrypoints where
  type ParameterEntrypointsDerivation AdminEntrypoints = EpdPlain

initAdminStorage :: Address -> AdminStorage
initAdminStorage admin = Just AdminStorage
  { admin = admin
  , pendingAdmin = Nothing
  , paused = False
  }

noAdminStorage :: AdminStorage
noAdminStorage = Nothing

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
