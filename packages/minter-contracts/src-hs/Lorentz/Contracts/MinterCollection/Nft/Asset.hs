-- | Lorentz bindings for @minter_collection/nft/fa2_multi_nft_asset.mligo@
module Lorentz.Contracts.MinterCollection.Nft.Asset
  ( Entrypoints(..)
  , Storage(..)
  , StorageWithTokenMetadata(..)
  , EntrypointsWithMint(..)
  , balanceOf
  , initNftContractStorageWithTokenMetadata
  ) where


import Fmt (Buildable(..), genericF, indentF, unlinesF)
import Lorentz

import qualified Lorentz.Contracts.FA2 as FA2
import qualified Lorentz.Contracts.MinterCollection.Nft.Token as NftToken
import qualified Lorentz.Contracts.SimpleAdmin as Admin
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2I
import Lorentz.Test (contractConsumer)

import qualified Michelson.Typed as T
import Lorentz.Test.Import (embedContractM)
import Morley.Nettest

import Lorentz.Contracts.MinterSdk(inBinFolder)

data MintTokenParam = MintTokenParam
  {
    tokenMetadata :: FA2.TokenMetadata 
  , owner :: Address
  }
  deriving stock (Show, Eq)
customGeneric "MintTokenParam" ligoCombLayout
deriving anyclass instance IsoValue MintTokenParam
deriving anyclass instance HasAnnotation MintTokenParam

data Entrypoints
  = Assets FA2.Parameter
  | Admin Admin.AdminEntrypoints
  deriving stock (Show, Eq)
customGeneric "Entrypoints" ligoLayout
deriving anyclass instance IsoValue Entrypoints
deriving anyclass instance HasAnnotation Entrypoints

instance ParameterHasEntrypoints Entrypoints where
  type ParameterEntrypointsDerivation Entrypoints = EpdDelegate

data EntrypointsWithMint
  = Entrypoints Entrypoints
  | Mint [MintTokenParam]
  deriving stock (Show, Eq)
customGeneric "EntrypointsWithMint" ligoLayout
deriving anyclass instance IsoValue EntrypointsWithMint
deriving anyclass instance HasAnnotation EntrypointsWithMint

instance ParameterHasEntrypoints EntrypointsWithMint where
  type ParameterEntrypointsDerivation EntrypointsWithMint = EpdDelegate


-- Note: Hardcoded to use 'PausableAdminOption' (Simple admin),
-- but should be generalized to work with any admin.
data StorageWithTokenMetadata = StorageWithTokenMetadata
  { assets' :: NftToken.StorageWithMetadata
  , admin' :: Admin.AdminStorage
  , metadata' :: BigMap MText ByteString
  }
  deriving stock (Show, Eq)
customGeneric "StorageWithTokenMetadata" ligoLayout
deriving anyclass instance IsoValue StorageWithTokenMetadata
deriving anyclass instance HasAnnotation StorageWithTokenMetadata

initNftContractStorageWithTokenMetadata :: Address -> StorageWithTokenMetadata
initNftContractStorageWithTokenMetadata admin = StorageWithTokenMetadata
  { admin' = Admin.initAdminStorage admin
  , metadata' = mempty
  , assets' = NftToken.initNftTokenStorageWithMetadata
  }

data Storage = Storage
  { assets :: NftToken.Storage
  , admin :: Admin.AdminStorage
  , metadata :: BigMap MText ByteString
  }
  deriving stock (Show, Eq)
customGeneric "Storage" ligoLayout
deriving anyclass instance IsoValue Storage
deriving anyclass instance HasAnnotation Storage
instance Buildable Storage where build = genericF


-- | Retrieve the FA2 balance for a given account.
balanceOf
  :: (HasCallStack, MonadNettest caps base m, ToAddress addr)
  => ContractHandler EntrypointsWithMint StorageWithTokenMetadata -> FA2I.TokenId -> addr -> m Natural
balanceOf fa2 tokenId account = do
  consumer <- originateSimple "balance-response-consumer" [] (contractConsumer @[FA2I.BalanceResponseItem])
  call fa2 (Call @"Balance_of") (FA2I.mkFA2View [FA2I.BalanceRequestItem (toAddress account) tokenId] consumer)
  consumerStorage <- getStorage consumer

  case consumerStorage of
    [[balanceResponseItem]] -> pure $ FA2I.briBalance balanceResponseItem
    _ -> failure $ unlinesF
          [ "Expected consumer storage to have exactly 1 balance response, with exactly 1 item."
          , "Consumer storage:"
          , indentF 2 $ build consumerStorage
          ]
