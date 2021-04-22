-- | Lorentz bindings for the editions contract.
module Lorentz.Contracts.MinterCollection.Editions
  ( EditionInfo
  , EditionId(..)
  , EditionMetadata(..)
  , Storage(..)
  , MintEditionParam(..)
  , DistributeEditionParam(..)
  , Entrypoints(..)
  , editionsContract
  ) where

import Fmt (Buildable(..), genericF)
import Lorentz
import Michelson.Test.Import (embedContractM)
import qualified Michelson.Typed as T

import qualified Lorentz.Contracts.MinterCollection.Nft.Asset as NftAsset
import Lorentz.Contracts.MinterSdk (inBinFolder)

type EditionInfo = Map MText ByteString

newtype EditionId = EditionId { unEditionId :: Natural }
  deriving stock (Generic, Eq, Ord, Show)
  deriving newtype (IsoValue, HasAnnotation, Num, Buildable)

data EditionMetadata = EditionMetadata
  { creator :: Address
  , editionInfo :: EditionInfo
  , numberOfEditions :: Natural
  , numberOfEditionsToDistribute :: Natural
  }
  deriving stock (Show, Eq)
customGeneric "EditionMetadata" rightComb
deriving anyclass instance IsoValue EditionMetadata
deriving anyclass instance HasAnnotation EditionMetadata
instance Buildable EditionMetadata where build = genericF

data Storage = Storage
  { nextEditionId :: EditionId
  , maxEditionsPerRun :: Natural
  , editionsMetadata :: BigMap EditionId EditionMetadata
  , nftAssetStorage :: NftAsset.Storage
  }
  deriving stock (Show, Eq)
customGeneric "Storage" ligoLayout
deriving anyclass instance IsoValue Storage
deriving anyclass instance HasAnnotation Storage
instance Buildable Storage where build = genericF

data MintEditionParam = MintEditionParam
  { editionInfo :: EditionInfo
  , numberOfEditions :: Natural
  }
  deriving stock (Show, Eq)
customGeneric "MintEditionParam" ligoLayout
deriving anyclass instance IsoValue MintEditionParam
deriving anyclass instance HasAnnotation MintEditionParam

data DistributeEditionParam = DistributeEditionParam
  { editionId :: EditionId
  , receivers :: [Address]
  }
  deriving stock (Show, Eq)
customGeneric "DistributeEditionParam" ligoLayout
deriving anyclass instance IsoValue DistributeEditionParam
deriving anyclass instance HasAnnotation DistributeEditionParam

data Entrypoints
  = FA2 NftAsset.Entrypoints
  | Mint_editions [MintEditionParam]
  | Distribute_editions [DistributeEditionParam]
  deriving stock (Show, Eq)
customGeneric "Entrypoints" ligoLayout
deriving anyclass instance IsoValue Entrypoints
deriving anyclass instance HasAnnotation Entrypoints

instance ParameterHasEntrypoints Entrypoints where
  type ParameterEntrypointsDerivation Entrypoints = EpdRecursive

----------------------------------------------------------------------------
-- Contract
----------------------------------------------------------------------------

editionsContract :: T.Contract (ToT Entrypoints) (ToT Storage)
editionsContract = $$(embedContractM (inBinFolder "fa2_multi_nft_token_editions.tz"))
