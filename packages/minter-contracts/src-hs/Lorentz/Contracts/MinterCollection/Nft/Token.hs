-- | Lorentz bindings for @minter_collection/nft/fa2_multi_nft_token.mligo@
module Lorentz.Contracts.MinterCollection.Nft.Token
  ( Storage(..),
    StorageWithMetadata(..),
    initNftTokenStorageWithMetadata
  ) where


import Lorentz

import qualified Lorentz.Contracts.FA2 as FA2
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2I
import Util ()
import Fmt (Buildable(..), genericF)

data StorageWithMetadata = StorageWithMetadata
  { ledger :: BigMap FA2I.TokenId Address
  , nextTokenId :: Natural
  , operators :: FA2.OperatorStorage
  , tokenMetadata :: BigMap FA2I.TokenId FA2.TokenMetadata 
  }
  deriving stock (Show, Eq)
customGeneric "StorageWithMetadata" ligoLayout
deriving anyclass instance IsoValue StorageWithMetadata
deriving anyclass instance HasAnnotation StorageWithMetadata

initNftTokenStorageWithMetadata :: StorageWithMetadata
initNftTokenStorageWithMetadata = StorageWithMetadata
  {  ledger = mempty
   , operators = mempty
   , nextTokenId = 0
   , tokenMetadata = mempty
  }

data Storage = Storage
  { ledger :: BigMap FA2I.TokenId Address
  , operators :: FA2.OperatorStorage
  }
  deriving stock (Show, Eq)
customGeneric "Storage" ligoLayout
deriving anyclass instance IsoValue Storage
deriving anyclass instance HasAnnotation Storage
instance Buildable Storage where build = genericF
