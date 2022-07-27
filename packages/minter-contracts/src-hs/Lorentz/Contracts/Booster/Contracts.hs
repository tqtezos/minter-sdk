-- | English auction contracts.
module Lorentz.Contracts.Booster.Contracts where

import Lorentz

import Lorentz.Contracts.MinterSdk
import qualified Lorentz.Contracts.NoAllowlist as NoAllowlist
import Lorentz.Contracts.Booster.Redeemer
import Lorentz.Test.Import (embedContractM)

-- AuctionFA2
----------------------------------------------------------------------------

boosterContract
  :: Contract
      BoosterEntrypoints
      BoosterStorage
boosterContract =
  $$(embedContractM (inBinFolder "booster_distributor.tz"))
