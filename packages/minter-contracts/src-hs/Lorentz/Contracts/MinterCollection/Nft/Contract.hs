-- | Lorentz bindings for NFT multi-asset contract
module Lorentz.Contracts.MinterCollection.Nft.Contract where

import Lorentz (Contract)
import Lorentz.Test.Import (embedContractM)

import Lorentz.Contracts.MinterSdk (inBinFolder)

import Lorentz.Contracts.MinterCollection.Nft.Types

nftContract :: Contract NftEntrypoints NftStorage
nftContract = $$(embedContractM (inBinFolder "fa2_multi_nft_asset.tz"))

