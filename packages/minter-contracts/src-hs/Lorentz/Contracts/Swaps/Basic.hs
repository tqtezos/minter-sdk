-- | Lorentz bindings for the swaps contract.
module Lorentz.Contracts.Swaps.Basic where

import Lorentz

import Lorentz.Contracts.MinterSdk
import Lorentz.Contracts.Spec.FA2Interface
import Michelson.Test.Import (embedContractM)
import qualified Michelson.Typed as T

-- Types
----------------------------------------------------------------------------

newtype SwapId = SwapId Natural
  deriving stock (Show, Eq, Ord)
  deriving newtype (IsoValue, HasAnnotation)

data FA2Token = FA2Token
  { tokenId :: TokenId
  , amount :: Natural
  }

customGeneric "FA2Token" ligoCombLayout
deriving anyclass instance IsoValue FA2Token
deriving anyclass instance HasAnnotation FA2Token

data FA2Assets = FA2Assets
  { fa2Address :: Address
  , tokens :: [FA2Token]
  }

customGeneric "FA2Assets" ligoCombLayout
deriving anyclass instance IsoValue FA2Assets
deriving anyclass instance HasAnnotation FA2Assets

data SwapOffer = SwapOffer
  { assetsOffered :: [FA2Assets]
  , assetsRequested :: [FA2Assets]
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

data SwapEntrypoints
  = Start SwapOffers
  | Cancel SwapId
  | Accept SwapId

customGeneric "SwapEntrypoints" ligoLayout
deriving anyclass instance IsoValue SwapEntrypoints
deriving anyclass instance HasAnnotation SwapEntrypoints

instance ParameterHasEntrypoints SwapEntrypoints where
  type ParameterEntrypointsDerivation SwapEntrypoints = EpdPlain

data SwapStorage = SwapStorage
  { nextSwapId :: SwapId
  , swaps :: BigMap SwapId SwapInfo
  }

customGeneric "SwapStorage" ligoLayout
deriving anyclass instance IsoValue SwapStorage
deriving anyclass instance HasAnnotation SwapStorage

incrementSwapId :: SwapId -> SwapId 
incrementSwapId (SwapId n) = SwapId (n + 1)

initSwapId :: SwapId 
initSwapId = SwapId 1

initSwapStorage :: SwapStorage
initSwapStorage = SwapStorage
  { nextSwapId = initSwapId
  , swaps = mempty
  }

-- Contract
----------------------------------------------------------------------------

swapsContract :: T.Contract (ToT SwapEntrypoints) (ToT SwapStorage)
swapsContract = $$(embedContractM (inBinFolder "fa2_swap.tz"))

-- Errors
----------------------------------------------------------------------------

errSwapNotExist :: MText
errSwapNotExist = [mt|SWAP_NOT_EXIST|]

errSwapFinished :: MText
errSwapFinished = [mt|SWAP_NOT_EXIST|]

errSwapCancelled :: MText
errSwapCancelled = [mt|SWAP_NOT_EXIST|]
-- ↑ The contract does not actually distinguish these cases

errNotSwapSeller :: MText
errNotSwapSeller = [mt|NOT_SWAP_SELLER|]

errSwapOfferedFA2Invalid :: MText
errSwapOfferedFA2Invalid = [mt|SWAP_OFFERED_FA2_INVALID|]

errSwapRequestedFA2Invalid :: MText
errSwapRequestedFA2Invalid = [mt|SWAP_REQUESTED_FA2_INVALID|]

errSwapRequestedFA2BalanceInvalid :: (MText, ()) 
errSwapRequestedFA2BalanceInvalid = ([mt|FA2_NOT_OPERATOR|], ())

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