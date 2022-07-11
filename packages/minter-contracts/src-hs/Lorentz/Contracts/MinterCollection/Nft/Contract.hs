module Lorentz.Contracts.MinterCollection.Nft.Contract
  ( nftContract
  ) where

import Lorentz
import Lorentz.Test.Import (embedContractM)
import Lorentz.Contracts.MinterSdk (inBinFolder)
import Lorentz.Contracts.MinterCollection.Nft.Asset

nftContract :: Contract EntrypointsWithMint StorageWithTokenMetadata
nftContract = $$(embedContractM (inBinFolder "fa2_multi_nft_asset.tz"))