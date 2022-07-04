-- | Lorentz editions contract.
module Lorentz.Contracts.MinterCollection.Editions.Contracts
  ( editionsContract
  ) where

import Lorentz
import Lorentz.Test.Import (embedContractM)

import Lorentz.Contracts.MinterCollection.Editions.Types
import Lorentz.Contracts.MinterSdk (inBinFolder)

editionsContract :: Contract Entrypoints Storage
editionsContract = $$(embedContractM (inBinFolder "fa2_multi_nft_token_editions.tz"))
