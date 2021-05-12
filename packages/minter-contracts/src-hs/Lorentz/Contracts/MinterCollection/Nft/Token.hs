-- | Lorentz bindings for @minter_collection/nft/fa2_multi_nft_token.mligo@
module Lorentz.Contracts.MinterCollection.Nft.Token
  ( Storage(..)
  ) where


import Fmt (Buildable(..), genericF)
import Lorentz

import qualified Lorentz.Contracts.FA2 as FA2
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2I
import Util ()

data Storage = Storage
  { ledger :: BigMap FA2I.TokenId Address
  , operators :: FA2.OperatorStorage
  }
  deriving stock (Show, Eq)
customGeneric "Storage" ligoLayout
deriving anyclass instance IsoValue Storage
deriving anyclass instance HasAnnotation Storage
instance Buildable Storage where build = genericF
