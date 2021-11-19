-- | Tests on the swaps contract with offchain acceptance of a swap.
module Test.Swaps.SwapPermit where

import Prelude hiding (swap, toStrict)

import qualified Data.Sized as Sized (toList)

import Morley.Nettest

import Hedgehog (Property, property, forAll)

import Michelson.Interpret.Pack 

import Lorentz.Contracts.Swaps.Allowlisted
import Lorentz.Contracts.Swaps.Basic
import Lorentz.Contracts.Swaps.SwapPermit
import Lorentz.Value

import Test.Swaps.Basic
import Test.Swaps.Util
import Test.Util

import Tezos.Crypto

hprop_Sending_fake_permit_to_offchain_accept_fails :: Property
hprop_Sending_fake_permit_to_offchain_accept_fails =
  property $ do
    clevelandProp $ do
      setup <- doFA2Setup
      let admin ::< alice ::< SNil = sAddresses setup
      let tokenId1 ::< SNil = sTokens setup
      swap <- originateOffchainSwap admin
      swapId <- (\(SwapId n) -> n) . 
                nextSwapId . 
                swapStorage <$>
                fromVal @AllowlistedSwapStorage <$> 
                getStorage' swap 
      fa2 <- originateFA2 "fa2" setup [swap]
      withSender admin $
        call swap (Call @"Update_allowed") (mkAllowlistSimpleParam [fa2])
      withSender admin $ do 
        call swap (Call @"Start") $ mkSingleOffer SwapOffer
          { assetsOffered = []
          , assetsRequested = [mkFA2Assets fa2 [(tokenId1, 1)]]
          }
      missignedBytes <- fst <$> mkPermitToForge swapId swap
      withSender admin $ do
        offchainAcceptForged alice swap `expectFailure` failedWith swap
          ([mt|MISSIGNED|], missignedBytes)

hprop_Offchain_accept_not_admin_submitted_fails :: Property
hprop_Offchain_accept_not_admin_submitted_fails =
  property $ do
    clevelandProp $ do
      setup <- doFA2Setup
      let admin ::< alice ::< SNil = sAddresses setup
      let tokenId1 ::< SNil = sTokens setup
      swap <- originateOffchainSwap admin
      fa2 <- originateFA2 "fa2" setup [swap]
      withSender admin $
        call swap (Call @"Update_allowed") (mkAllowlistSimpleParam [fa2])
      withSender admin $ do 
        call swap (Call @"Start") $ mkSingleOffer SwapOffer
          { assetsOffered = []
          , assetsRequested = [mkFA2Assets fa2 [(tokenId1, 1)]]
          }
      withSender alice $ do
        offchainAccept alice swap `expectFailure` failedWith swap
          errNotAdmin

hprop_Consecutive_offchain_accept_equals_iterative_accept :: Property
hprop_Consecutive_offchain_accept_equals_iterative_accept =
    property $ do
      TestData{numOffers,token1Offer, token2Offer, token1Request, token2Request} <- forAll genTestData
      clevelandProp $ do
        setup <- doFA2Setup @("addresses" :# 50) @("tokens" :# 2)
        let admin1 ::< admin2 ::< alice ::< remainingAddresses = sAddresses setup
        let addresses = take (fromIntegral numOffers) (Sized.toList remainingAddresses) 
        let tokenId1 ::< tokenId2 ::< SNil = sTokens setup
        swap1 <- originateOffchainSwap admin1
        swap2 <- originateOffchainSwap admin2
        fa2_1 <- originateFA2 "fa2_1" setup [swap1]
        fa2_2 <- originateFA2 "fa2_2" setup [swap2]
        withSender admin1 $
          call swap1 (Call @"Update_allowed") (mkAllowlistSimpleParam [fa2_1])
        withSender admin2 $
          call swap2 (Call @"Update_allowed") (mkAllowlistSimpleParam [fa2_2])
        withSender admin1 $ do 
          call swap1 (Call @"Start") $ mkNOffers numOffers SwapOffer
               { assetsOffered = [mkFA2Assets fa2_1 [(tokenId1, token1Offer), (tokenId2, token2Offer)]]
               , assetsRequested = [mkFA2Assets fa2_1 [(tokenId1, token1Request), (tokenId2, token2Request)]]
               }
        withSender admin2 $ do 
          call swap2 (Call @"Start") $ mkNOffers numOffers SwapOffer
               { assetsOffered = [mkFA2Assets fa2_2 [(tokenId1, token1Offer), (tokenId2, token2Offer)]]
               , assetsRequested = [mkFA2Assets fa2_2 [(tokenId1, token1Request), (tokenId2, token2Request)]]
               }
        withSender admin1 $ do
          offchainAcceptAllConsecutive addresses swap1
        withSender admin2 $ do
          offchainAcceptBatch addresses swap2
      
        swapStorage1 <-  toVal <$>
                         swapStorage <$>
                         fromVal @AllowlistedSwapStorage <$> 
                         getStorage' swap1
        swapStorage2 <-  toVal <$>
                         swapStorage <$>
                         fromVal @AllowlistedSwapStorage <$> 
                         getStorage' swap2
        swapStorage1 @== swapStorage2

hprop_Accepting_with_zero_balance_fails :: Property
hprop_Accepting_with_zero_balance_fails =
    property $ do
      clevelandProp $ do
          setup <- doFA2Setup
          let admin ::< SNil = sAddresses setup
          let tokenId1 ::< tokenId2 ::< SNil = sTokens setup
          swap <- originateOffchainSwap admin
          fa2 <- originateFA2 "fa2" setup [swap]
          withSender admin $
            call swap (Call @"Update_allowed") (mkAllowlistSimpleParam [fa2])
          addressWithZeroBalance <- newAddress "test"
          withSender admin $
            call swap (Call @"Start") $ mkSingleOffer SwapOffer
              { assetsOffered = [mkFA2Assets fa2 [(tokenId1, 10)]]
              , assetsRequested = [mkFA2Assets fa2 [(tokenId2, 5)]]
              }
          withSender admin
            (offchainAccept addressWithZeroBalance swap
              `expectFailure` failedWith fa2 errSwapRequestedFA2BalanceInvalid)  

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- N addresses accept all N assets in a sale conseutively, and then all N are confirmed
offchainAcceptAllConsecutive :: (HasCallStack, MonadEmulated caps base m) => [Address] -> TAddress PermitSwapEntrypoints -> m ()
offchainAcceptAllConsecutive addresses contract = do
  forM_ addresses $ \buyer -> do
      offchainAccept buyer contract

offchainAcceptBatch :: (HasCallStack, MonadEmulated caps base m) => [Address] -> TAddress PermitSwapEntrypoints -> m ()
offchainAcceptBatch buyers contract = do
  param <- forM buyers $ \buyer -> do
    buyerPK <- getPublicKey buyer
    unsigned <- mkPermitToSign swapId contract
    signature <- signBytes unsigned buyer
    return OffchainAcceptParam { 
        swapId = swapId
      , permit = Permit
          {
            signerKey = buyerPK
          , signature = signature
          } 
      } 
  call contract (Call @"Offchain_accept") (toList param) 
  where swapId = 1

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