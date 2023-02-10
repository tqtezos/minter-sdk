{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE InstanceSigs #-}

module Test.Util
  ( (-:)
  , type (:#)
  , pattern (::<)
  , pattern SNil
  , FA2Setup (..)
  , doFA2Setup
  , originateFA2
  , originateFA2WithGlobalOperators
  , assertingBalanceDeltas
  , assertingBalanceDeltas'
  , balanceOf
  , mkAllowlistSimpleParam
  , originateWithAdmin
  , WrappedValue(..)

  -- * Property-based tests
  , clevelandProp
  , iterateM

    -- Re-exports
  , Sized
  ) where

import qualified Data.Foldable as F
import qualified Data.Map as Map
import Data.Maybe
import Data.Sized (Sized)
import qualified Data.Sized as Sized
import Data.Type.Natural.Lemma.Order (type (<))
import Data.Type.Ordinal (ordToNatural)
import Fmt (build, indentF, unlinesF, (+|), (|+))
import GHC.TypeLits (Symbol)
import GHC.TypeNats (Nat, type (+))
import Hedgehog (Gen, MonadTest)

import Lorentz.Errors
import Lorentz.Test.Consumer
import Lorentz.Value
import Michelson.Typed.Scope (ConstantScope)
import Michelson.Typed.Sing (KnownT)

import qualified Indigo.Contracts.FA2Sample as FA2
import Lorentz.Contracts.FA2
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2

import qualified Lorentz.Contracts.MinterCollection.Ft.Asset as FtAsset
import qualified Lorentz.Contracts.MinterCollection.Ft.Token as FtToken
import qualified Lorentz.Contracts.PausableAdminOption as PausableAdminOption
import Morley.Nettest
import Morley.Nettest.Pure (PureM, runEmulated)

-- | An alias for pair constructor.
infix 0 -:
(-:) :: a -> b -> (a, b)
(-:) = (,)

-- | Helper type that attaches a name to a numeric type literal.
data (:#) :: Symbol -> Nat -> Type

-- | Pattern-match on a list of fixed size that has some elements in it.
--
-- Unlike 'Sized.:<', this pattern requires the list to be non-empty via
-- the type-system and thus is total.
infixr 2 ::<
pattern (::<) :: (((1 + n) ~ m), (0 < m), KnownNat m) => a -> Sized [] n a -> Sized [] m a
pattern a ::< l <- ((Sized.head &&& Sized.tail) -> (a, l))
{-# COMPLETE (::<) #-}

-- | Pattern-match on an empty list of fixed size.
--
-- Unlike 'Sized.:<', this pattern requires the list to be empty via the
-- type-system and thus is total.
pattern SNil :: Sized [] 0 a
pattern SNil <- _
{-# COMPLETE SNil #-}

-- | Test setup.
--
-- We remember sizes of all entries lists in types because that facilitates
-- allocating exactly as many entities as necessary for the given test suite.
data FA2Setup addrsNum tokensNum = FA2Setup
  { sAddresses :: Sized [] addrsNum Address
  , sTokens :: Sized [] tokensNum FA2.TokenId
  } deriving stock (Show)

{- | Prepare all the operated entities.

Note that number of addresses and other entities may be inferred automatically,
so you should bind all the fields of returned 'FA2Setup'. For instance:

@
scenario = do
  setup <- doFA2Setup
  let addr1 ::< addr2 ::< SNil = sAddresses setup
  -- ↑ Here compiler figures out that exactly 2 addresses should be allocated
  -- during setup...

  let tokenId ::< SNil = sTokens setup
  -- ↑ ...and only one token.

  ...
@

Another option is to explicitly annotate the 'doFA2Setup' call:

@
scenario = do
  setup <- doFA2Setup @("addresses" :# 2) @("tokens" :# 1)
  ...
@
-}
doFA2Setup
  :: forall addrsArg tokensArg addrsNum tokensNum caps base m.
     ( MonadNettest caps base m
     , KnownNat addrsNum, addrsArg ~ ("addresses" :# addrsNum)
     , KnownNat tokensNum, tokensArg ~ ("tokens" :# tokensNum)
     )
  => m (FA2Setup addrsNum tokensNum)
doFA2Setup = do
  let aliases = Sized.generate' $ \i -> fromString ("fa2-addr-" <> show (ordToNatural i))
  sAddresses <- mapM newAddress aliases
  let sTokens = Sized.generate' $ \i -> FA2.TokenId (ordToNatural i)
  return FA2Setup{..}

-- | Originate a trivial FA2 contract suitable for testing the provided swaps
-- contract:
-- * Some money will be put on the addresses from setup, the swaps contract
--   will be made operator of those addresses.
-- * The tokenIds from setup will be supported by the originated contract.
originateFA2
  :: MonadNettest caps base m
  => AliasHint
  -> FA2Setup addrsNum tokensNum
  -> [ContractHandler contractParam contractStorage]
  -> m (ContractHandler FA2.FA2SampleParameter FA2.Storage)
originateFA2 name FA2Setup{..} contracts = do
  fa2 <- originateSimple name
    FA2.Storage
    { sLedger = BigMap $ Map.fromList do
        -- put money on several tokenIds for each given address
        addr <- F.toList sAddresses
        tokenId <- F.toList sTokens
        pure ((addr, tokenId), 1000)
    , sOperators = BigMap $ Map.fromList do
        owner <- F.toList sAddresses
        operator <- contracts
        pure ((owner, toAddress operator), ())
    , sTokenMetadata = mempty
    }
    (FA2.fa2Contract def
      { FA2.cAllowedTokenIds = F.toList sTokens
      }
    )
  return fa2

originateFA2WithGlobalOperators
  :: MonadNettest caps base m
  => AliasHint
  -> FA2Setup addrsNum tokensNum
  -> Set Address
  -> Address
  -> [ContractHandler contractParam contractStorage]
  -> m (ContractHandler FtAsset.LimitedWithGlobalOperatorsEntrypoints FtAsset.LimitedStorageWithGlobalOperators)
originateFA2WithGlobalOperators name FA2Setup{..} globalOperators admin operatorContracts = do
  fa2 <- originateTypedSimple name
    FtAsset.LimitedStorageWithGlobalOperators
    {
      assets = FtToken.LimitedStorageWithGlobalOperators
        {
        ledger = BigMap $ Map.fromList do
          -- put money on several tokenIds for each given address
          addr <- F.toList sAddresses
          tokenId <- F.toList sTokens
          pure ((addr, tokenId), 1000)
        , operators = BigMap $ Map.fromList do
          owner <- F.toList sAddresses
          operator <- operatorContracts
          tokenId <- F.toList sTokens
          pure ((OperatorKey owner (toAddress operator) tokenId), ())
        , tokenMetadata = BigMap $ Map.fromList do
          tokenId <- F.toList sTokens
          pure (tokenId, (TokenMetadata tokenId mempty))
        , globalOperators = globalOperators
        , nextTokenId = 0
        , totalTokenSupply = mempty
        },
      metadata = mempty,
      admin = fromJust $ PausableAdminOption.initAdminStorage admin
    }
    (FtAsset.limitedWithGlobalOperatorsContract)
  return fa2

-- | Given a FA2 contract address, checks that balances of the given
-- address/token_ids change by the specified delta values.
assertingBalanceDeltas
  :: (MonadNettest caps base m, HasCallStack)
  => ContractHandler FA2.FA2SampleParameter storage
  -> [((Address, FA2.TokenId), Integer)]
  -> m a
  -> m a
assertingBalanceDeltas fa2 indicedDeltas action = do
  consumer <- originateSimple "consumer" [] contractConsumer

  pullBalance consumer
  res <- action
  pullBalance consumer

  balancesRes <- map (map FA2.briBalance) <$> getStorage consumer
  (balancesAfter, balancesBefore) <- case balancesRes of
    [balancesAfter, balancesBefore] ->
      return (balancesAfter, balancesBefore)
    other -> failure $ "Unexpected consumer storage: " +| other |+ ""

  forM_ (zip3 indicedDeltas balancesBefore balancesAfter) $
    \(((addr, tokenId), expected), actualBefore, actualAfter) -> do
      let actual = toInteger actualAfter - toInteger actualBefore
      assert (expected == actual) $
        "For address " +| addr |+ "\n(token id = " +| tokenId |+ ")\n\
        \got unexpected balance delta: \
        \expected " +| expected |+ ", got " +| actual |+ ""
  return res
    where
      pullBalance
        :: MonadNettest base caps m
        => ContractHandler [FA2.BalanceResponseItem] storage -> m ()
      pullBalance consumer = do
        let tokenRefs = map fst indicedDeltas
        call fa2 (Call @"Balance_of") $
          FA2.mkFA2View
            (uncurry FA2.BalanceRequestItem <$> tokenRefs)
            consumer

-- | Given a FA2 contract address, checks that balances of the given
-- address/token_ids change by the specified delta values.
assertingBalanceDeltas'
  :: (MonadNettest caps base m, HasCallStack)
  => ContractHandler FtAsset.LimitedWithGlobalOperatorsEntrypoints st
  -> [((Address, FA2.TokenId), Integer)]
  -> m a
  -> m a
assertingBalanceDeltas' fa2 indicedDeltas action = do
  consumer <- originateSimple "consumer" [] contractConsumer

  pullBalance consumer
  res <- action
  pullBalance consumer

  balancesRes <- map (map FA2.briBalance) <$>
    getStorage consumer
  (balancesAfter, balancesBefore) <- case balancesRes of
    [balancesAfter, balancesBefore] ->
      return (balancesAfter, balancesBefore)
    other -> failure $ "Unexpected consumer storage: " +| other |+ ""

  forM_ (zip3 indicedDeltas balancesBefore balancesAfter) $
    \(((addr, tokenId), expected), actualBefore, actualAfter) -> do
      let actual = toInteger actualAfter - toInteger actualBefore
      assert (expected == actual) $
        "For address " +| addr |+ "\n(token id = " +| tokenId |+ ")\n\
        \got unexpected balance delta: \
        \expected " +| expected |+ ", got " +| actual |+ ""
  return res
    where
      pullBalance
        :: MonadNettest base caps m
        => ContractHandler [FA2.BalanceResponseItem] st -> m ()
      pullBalance consumer = do
        let tokenRefs = map fst indicedDeltas
        call fa2 (Call @"Balance_of") $
          FA2.mkFA2View
            (uncurry FA2.BalanceRequestItem <$> tokenRefs)
            consumer

-- | Retrieve the FA2 balance for a given account.
balanceOf
  :: (HasCallStack, MonadNettest caps base m, ToAddress addr)
  => ContractHandler FA2.FA2SampleParameter storage -> FA2.TokenId -> addr -> m Natural
balanceOf fa2 tokenId account = do
  consumer <- originateSimple "balance-response-consumer" [] (contractConsumer @[FA2.BalanceResponseItem])
  call fa2 (Call @"Balance_of") (FA2.mkFA2View [FA2.BalanceRequestItem (toAddress account) tokenId] consumer)
  consumerStorage <- getStorage consumer

  case consumerStorage of
    [[balanceResponseItem]] -> pure $ FA2.briBalance balanceResponseItem
    _ -> failure $ unlinesF
          [ "Expected consumer storage to have exactly 1 balance response, with exactly 1 item."
          , "Consumer storage:"
          , indentF 2 $ build consumerStorage
          ]

-- | Construct allowlist for passing to allowlist overriding entrypoint.
mkAllowlistSimpleParam :: [ContractHandler p s] -> BigMap Address ()
mkAllowlistSimpleParam = mconcat . map (\a -> one (toAddress a, ()))

-- | Originate the a contract and admin for it.
originateWithAdmin
  :: MonadNettest caps base m
  => (Address -> m (ContractHandler param storage))
  -> m (ContractHandler param storage, Address)
originateWithAdmin originateFn = do
  admin <- newAddress "admin"
  swaps <- originateFn admin
  return (swaps, admin)

-- | Create a hedgehog property-based test from a cleveland scenario.
clevelandProp :: (MonadIO m, MonadTest m) => EmulatedT PureM () -> m ()
clevelandProp = nettestTestProp . runEmulated . uncapsNettestEmulated

-- | Given a generator of values of type @a@ and an initial value,
-- repeatedly uses the generator to create a list of the given length,
-- feeding it the previously generated value at each iteration.
iterateM :: forall a. Int -> (a -> Gen a) -> a -> Gen [a]
iterateM 0 _ _ = pure []
iterateM len gen previous = do
  current <- gen previous
  (current :) <$> iterateM (len - 1) gen current


-- | Wrap an IsoValue type so that is can be used with expectError
newtype WrappedValue a = WrappedValue
  { unwrapValue :: a
  } deriving stock (Eq, Ord, Show)

-- | Note: these are undefined because they're not needed to use WrappedValue to test
instance Typeable a => ErrorHasDoc (WrappedValue a) where
  type ErrorRequirements _ = ()

  errorDocName = error "ErrorHasDoc (WrappedValue a): undefined errorDocName"
  errorDocMdCause = error "ErrorHasDoc (WrappedValue a): undefined errorDocMdCause"
  errorDocHaskellRep = error "ErrorHasDoc (WrappedValue a): undefined errorDocHaskellRep"
  errorDocDependencies = error "ErrorHasDoc (WrappedValue a): undefined errorDocDependencies"

instance (IsoValue a, Typeable a, ConstantScope (ToT a)) => IsError (WrappedValue a) where
  errorToVal :: WrappedValue a -> (forall t. ErrorScope t => Value t -> r) -> r
  errorToVal xs ys = isoErrorToVal (unwrapValue xs) ys

  errorFromVal :: forall t. (KnownT t) => Value t -> Either Text (WrappedValue a)
  errorFromVal = fmap WrappedValue . isoErrorFromVal @t @a

