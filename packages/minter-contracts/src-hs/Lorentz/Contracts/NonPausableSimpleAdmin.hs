-- | Lorentz bindings for the non-pausable admin contract.
module Lorentz.Contracts.NonPausableSimpleAdmin where

import Lorentz

-- Types
----------------------------------------------------------------------------

data AdminStorage = AdminStorage
  { admin :: Address
  , pendingAdmin :: Maybe Address
  }

customGeneric "AdminStorage" ligoLayout
deriving anyclass instance IsoValue AdminStorage
deriving anyclass instance HasAnnotation AdminStorage

data AdminEntrypoints
  = Set_admin Address
  | Confirm_admin

customGeneric "AdminEntrypoints" ligoLayout
deriving anyclass instance IsoValue AdminEntrypoints
deriving anyclass instance HasAnnotation AdminEntrypoints

instance ParameterHasEntrypoints AdminEntrypoints where
  type ParameterEntrypointsDerivation AdminEntrypoints = EpdPlain

initAdminStorage :: Address -> AdminStorage
initAdminStorage admin = AdminStorage
  { admin = admin
  , pendingAdmin = Nothing
  }

-- This empty slice is a workaround, so that all the declarations above and
-- their instances may be in the type environment in the TH splice below.
$(pure [])

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
