-- | Tests on the swaps contract with offchain acceptance of a swap.
module Test.Swaps.SwapPermit where

import Prelude hiding (swap, toStrict)

import Morley.Nettest

import Hedgehog (Property, property)

import Michelson.Interpret.Pack 

import Lorentz.Contracts.Swaps.Allowlisted
import Lorentz.Contracts.Swaps.Basic
import Lorentz.Contracts.Swaps.SwapPermit
import Lorentz.Value

import Test.Swaps.Util
import Test.Util

import Tezos.Crypto

hprop_Sending_fake_permit_to_offchain_accept_fails :: Property
hprop_Sending_fake_permit_to_offchain_accept_fails =
  property $ do
    clevelandProp $ do
      setup <- doFA2Setup
      let alice ::< bob ::< SNil = sAddresses setup
      let tokenId1 ::< SNil = sTokens setup
      (swap, admin) <- originateOffchainSwapWithAdmin
      swapId <- (\(SwapId n) -> n) . 
                nextSwapId . 
                swapStorage <$>
                fromVal @AllowlistedSwapStorage <$> 
                getStorage' swap 
      fa2 <- originateFA2 "fa2" setup [swap]
      withSender admin $
        call swap (Call @"Update_allowed") (mkAllowlistSimpleParam [fa2])
      withSender alice $ do 
        call swap (Call @"Start") $ mkSingleOffer SwapOffer
          { assetsOffered = []
          , assetsRequested = [mkFA2Assets fa2 [(tokenId1, 1)]]
          }
      missignedBytes <- fst <$> mkPermitToForge swapId swap
      withSender admin $ do
        offchainAcceptForged bob swap `expectFailure` failedWith swap
          ([mt|MISSIGNED|], missignedBytes)

hprop_Offchain_accept_not_admin_submitted_fails :: Property
hprop_Offchain_accept_not_admin_submitted_fails =
  property $ do
    clevelandProp $ do
      setup <- doFA2Setup
      let alice ::< bob ::< SNil = sAddresses setup
      let tokenId1 ::< SNil = sTokens setup
      (swap, admin) <- originateOffchainSwapWithAdmin
      fa2 <- originateFA2 "fa2" setup [swap]
      withSender admin $
        call swap (Call @"Update_allowed") (mkAllowlistSimpleParam [fa2])
      withSender alice $ do 
        call swap (Call @"Start") $ mkSingleOffer SwapOffer
          { assetsOffered = []
          , assetsRequested = [mkFA2Assets fa2 [(tokenId1, 1)]]
          }
      withSender bob $ do
        offchainAccept bob swap `expectFailure` failedWith swap
          errNotAdmin
----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

mkPermitToForge :: (HasCallStack, MonadEmulated caps base m) => Natural -> TAddress PermitSwapEntrypoints -> m (ByteString, PublicKey)
mkPermitToForge swapId contract = do 
  aliasAddress <- newAddress "forged"
  aliasPK <- getPublicKey aliasAddress
  unsignedPermit <- mkPermitToSign swapId contract 
  pure (unsignedPermit, aliasPK)

mkPermitToSign :: (HasCallStack, MonadEmulated caps base m) => Natural -> TAddress PermitSwapEntrypoints -> m ByteString
mkPermitToSign swapId contract = do 
  marketplaceChainId <- getChainId
  let unsigned = packValue' $ toVal ((marketplaceChainId, contractAddress), (0 :: Natural, swapIdHash))
  pure unsigned
  where swapIdHash = blake2b $ packValue' $ toVal swapId 
        contractAddress = toAddress contract

offchainAccept :: (HasCallStack, MonadEmulated caps base m) => Address -> TAddress PermitSwapEntrypoints -> m ()
offchainAccept buyer contract = do
  buyerPK <- getPublicKey buyer
  unsigned <- mkPermitToSign swapId contract
  signature <- signBytes unsigned buyer 
  call contract (Call @"Offchain_accept") 
    [OffchainAcceptParam
      {
        swapId = swapId
      , permit = Permit
          {
            signerKey = buyerPK
          , signature = signature
          } 
      }
    ]
  where swapId = 1

offchainAcceptForged :: (HasCallStack, MonadEmulated caps base m) => Address -> TAddress PermitSwapEntrypoints -> m ByteString
offchainAcceptForged buyer contract = do
  (unsigned, forgedPK) <- mkPermitToForge swapId contract
  signature <- signBytes unsigned buyer 
  (\() -> unsigned) <$> call contract (Call @"Offchain_accept") 
    [OffchainAcceptParam
      {
        swapId = swapId
      , permit = Permit
          {
            signerKey = forgedPK
          , signature = signature
          } 
      }
    ] 
  where swapId = 1