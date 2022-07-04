-- | Lorentz bindings for the swap with permit contract
module Lorentz.Contracts.Swaps.Collections where

import Lorentz

import Lorentz.Contracts.NonPausableSimpleAdmin
import Lorentz.Contracts.Spec.FA2Interface (TokenId(..))
import Lorentz.Contracts.Swaps.Basic hiding
  (SwapInfo, SwapOffer, SwapOffers, mkNOffers, mkSingleOffer)
import Tezos.Address (unsafeParseAddress)
-- Types
----------------------------------------------------------------------------

newtype CollectionId = CollectionId Natural
  deriving stock (Show, Eq, Ord)
  deriving newtype (IsoValue, HasAnnotation)

data SwapOffer = SwapOffer
  { assetsOffered :: [FA2Token]
  , assetsRequested :: [CollectionId]
  }

customGeneric "SwapOffer" ligoCombLayout
deriving anyclass instance IsoValue SwapOffer
deriving anyclass instance HasAnnotation SwapOffer

data SwapOffers = SwapOffers
  { swapOffer :: SwapOffer
  , remainingOffers :: Natural
  }

customGeneric "SwapOffers" ligoCombLayout
deriving anyclass instance IsoValue SwapOffers
deriving anyclass instance HasAnnotation SwapOffers

data SwapInfo = SwapInfo
  { swapOffers :: SwapOffers
  , seller :: Address
  }

customGeneric "SwapInfo" ligoCombLayout
deriving anyclass instance IsoValue SwapInfo
deriving anyclass instance HasAnnotation SwapInfo

incrementCollectionId :: CollectionId -> CollectionId
incrementCollectionId (CollectionId n) = CollectionId (n + 1)

initCollectionId :: CollectionId
initCollectionId = CollectionId 1

getCollectionId :: CollectionId -> Natural
getCollectionId (CollectionId n) = n

data Permit = Permit
  { signerKey :: PublicKey
  , signature :: Signature
  }

customGeneric "Permit" ligoCombLayout
deriving anyclass instance IsoValue Permit
deriving anyclass instance HasAnnotation Permit

nullAddress :: Address
nullAddress = unsafeParseAddress "tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU"

exampleFA2Address :: Address
exampleFA2Address = unsafeParseAddress "KT1T7ShxhtSRuhvhHeug6Sjc7W8irLmswEt7"

data CollectionsStorage = CollectionsStorage
  { nextSwapId :: SwapId
  , nextCollectionId :: CollectionId
  , swaps :: BigMap SwapId SwapInfo
  , burnAddress :: Address
  , collections :: BigMap CollectionId (Set TokenId)
  , fa2Address :: Address
  , admin :: AdminStorage
  }

customGeneric "CollectionsStorage" ligoLayout
deriving anyclass instance IsoValue CollectionsStorage
deriving anyclass instance HasAnnotation CollectionsStorage

initCollectionsStorage :: Address -> Address -> CollectionsStorage
initCollectionsStorage admin fa2Address = CollectionsStorage
  { nextSwapId = initSwapId
  , nextCollectionId = initCollectionId
  , swaps = mempty
  , burnAddress = nullAddress
  , collections = mempty
  , fa2Address = fa2Address
  , admin = initAdminStorage admin
  }

data AcceptParam = AcceptParam
  { swapId :: SwapId ,
    tokensSent :: Set (CollectionId, TokenId)
  }

customGeneric "AcceptParam" ligoLayout
deriving anyclass instance IsoValue AcceptParam
deriving anyclass instance HasAnnotation AcceptParam

data CollectionsEntrypoints
  = Start SwapOffers
  | Cancel SwapId
  | Accept AcceptParam
  | Add_collection (Set TokenId)
  | Admin AdminEntrypoints

customGeneric "CollectionsEntrypoints" ligoLayout
deriving anyclass instance IsoValue CollectionsEntrypoints
deriving anyclass instance HasAnnotation CollectionsEntrypoints

instance ParameterHasEntrypoints CollectionsEntrypoints where
  type ParameterEntrypointsDerivation CollectionsEntrypoints = EpdDelegate

data OffchainAcceptParam = OffchainAcceptParam
  { acceptParam :: AcceptParam
  , permit :: Permit
  }

customGeneric "OffchainAcceptParam" ligoCombLayout
deriving anyclass instance IsoValue OffchainAcceptParam
deriving anyclass instance HasAnnotation OffchainAcceptParam

data OffchainCollectionsEntrypoints
  = BaseSwap CollectionsEntrypoints
  | Offchain_accept [OffchainAcceptParam]

customGeneric "OffchainCollectionsEntrypoints" ligoLayout
deriving anyclass instance IsoValue OffchainCollectionsEntrypoints
deriving anyclass instance HasAnnotation OffchainCollectionsEntrypoints

instance ParameterHasEntrypoints OffchainCollectionsEntrypoints where
  type ParameterEntrypointsDerivation OffchainCollectionsEntrypoints = EpdDelegate

-- Errors
----------------------------------------------------------------------------
errSwapNotExist :: MText
errSwapNotExist = [mt|SWAP_NOT_EXIST|]

errSwapFinished :: MText
errSwapFinished = [mt|SWAP_NOT_EXIST|]

errSwapCancelled :: MText
errSwapCancelled = [mt|SWAP_NOT_EXIST|]
-- ↑ The contract does not actually distinguish these cases

errSwapOfferedFA2Invalid :: MText
errSwapOfferedFA2Invalid = [mt|SWAP_OFFERED_FA2_INVALID|]

errSwapRequestedFA2Invalid :: MText
errSwapRequestedFA2Invalid = [mt|SWAP_REQUESTED_FA2_INVALID|]

errTokensSentInvalid :: MText
errTokensSentInvalid = [mt|TOKENS_SENT_INVALID|]

errSwapRequestedFA2BalanceInvalid :: Natural -> Natural -> (MText, Natural, Natural)
errSwapRequestedFA2BalanceInvalid requested actual = ([mt|FA2_INSUFFICIENT_BALANCE|], requested, actual)

errNotAdmin :: MText
errNotAdmin = [mt|NOT_AN_ADMIN|]

-- Helpers
----------------------------------------------------------------------------

mkNOffers :: Natural -> SwapOffer -> SwapOffers
mkNOffers n s = SwapOffers
  {
    swapOffer = s
  , remainingOffers = n
  }

mkSingleOffer :: SwapOffer -> SwapOffers
mkSingleOffer = mkNOffers 1
