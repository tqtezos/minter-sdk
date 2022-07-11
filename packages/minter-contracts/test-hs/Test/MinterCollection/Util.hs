module Test.MinterCollection.Util where
import Lorentz.Value
import qualified Michelson.Typed as T
import Morley.Nettest

import qualified Lorentz.Contracts.MinterCollection.Nft.Asset as NftAsset
import qualified Lorentz.Contracts.MinterCollection.Nft.Contract as NftContract

originateNftContract
  :: MonadNettest caps base m
  => Address
  -> m $ ContractHandler 
         NftAsset.EntrypointsWithMint
         NftAsset.StorageWithTokenMetadata
originateNftContract admin = do
  originateSimple "nft-asset"
    (NftAsset.initNftContractStorageWithTokenMetadata admin)
    NftContract.nftContract