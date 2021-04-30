-- | Lorentz bindings for @minter_collection/nft/fa2_multi_nft_asset.mligo@
module Lorentz.Contracts.MinterCollection.Nft.Asset
  ( Entrypoints(..)
  , Storage(..)
  ) where


import Fmt (Buildable(..), genericF)
import Lorentz

import qualified Lorentz.Contracts.FA2 as FA2
import qualified Lorentz.Contracts.MinterCollection.Nft.Token as NftToken
import qualified Lorentz.Contracts.PausableAdminOption as Admin

data Entrypoints
  = Assets FA2.Parameter
  | Admin Admin.AdminEntrypoints
  deriving stock (Show, Eq)
customGeneric "Entrypoints" ligoLayout
deriving anyclass instance IsoValue Entrypoints
deriving anyclass instance HasAnnotation Entrypoints

-- Note: Hardcoded to use 'PausableAdminOption' (Simple admin),
-- but should be generalized to work with any admin.
data Storage = Storage
  { assets :: NftToken.Storage
  , admin :: Admin.AdminStorageRecord
  , metadata :: BigMap MText ByteString
  }
  deriving stock (Show, Eq)
customGeneric "Storage" ligoLayout
deriving anyclass instance IsoValue Storage
deriving anyclass instance HasAnnotation Storage
instance Buildable Storage where build = genericF
