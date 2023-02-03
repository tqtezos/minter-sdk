module Lorentz.Contracts.MinterCollection.Nft.Contract
  ( nftContract
  ) where

import Lorentz
import Lorentz.Test.Import (embedContractM)
import Lorentz.Contracts.MinterSdk (inBinFolder)
import qualified Lorentz.Contracts.MinterCollection.Nft.Asset as NftAsset

nftContract :: Contract NftAsset.EntrypointsWithMint NftAsset.StorageWithTokenMetadata
nftContract = $$(embedContractM (inBinFolder "fa2_multi_nft_asset.tz"))