{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Test.Util
  ( (-:)
  , type (:#)
  , pattern (::<)
  , pattern SNil
  , FA2Setup (..)
  , doFA2Setup
  , assertingBalanceDeltas
  , mkAllowlistParam

    -- Re-exports
  , Sized
  ) where

import Data.Kind (Type)
import Data.Sized (Sized)
import qualified Data.Sized as Sized
import Data.Type.Natural.Lemma.Order (type (<))
import Data.Type.Ordinal (ordToNatural)
import Fmt ((+|), (|+))
import GHC.TypeLits (Symbol)
import GHC.TypeNats (Nat, type (+))

import Lorentz.Test.Consumer
import Lorentz.Value

import qualified Indigo.Contracts.FA2Sample as FA2
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Morley.Nettest

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

  let tokenId :< Nil = sTokens setup
  -- ↑ ...and only one token.

  ...
@

Another option is to explicitly annotate the 'doFA2Setup' call:

@
scenario = do
  setup <- toSetup @("addresses" :# 2) @("tokens" :# 1)
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

-- | Given a FA2 contract address, checks that balances of the given
-- address/token_ids change by the specified delta values.
assertingBalanceDeltas
  :: (MonadNettest caps base m, HasCallStack)
  => TAddress FA2.FA2SampleParameter
  -> [((Address, FA2.TokenId), Integer)]
  -> m a
  -> m a
assertingBalanceDeltas fa2 indicedDeltas action = do
  consumer <- originateSimple "consumer" [] contractConsumer

  pullBalance consumer
  res <- action
  pullBalance consumer

  balancesRes <- map (map FA2.briBalance) . fromVal <$>
    getStorage (AddressResolved $ toAddress consumer)
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
        => TAddress [FA2.BalanceResponseItem] -> m ()
      pullBalance consumer = do
        let tokenRefs = map fst indicedDeltas
        call fa2 (Call @"Balance_of") $
          FA2.mkFA2View
            (uncurry FA2.BalanceRequestItem <$> tokenRefs)
            consumer

-- | Construct allowlist for passing to allowlist overriding entrypoint.
mkAllowlistParam :: [Address] -> BigMap Address ()
mkAllowlistParam = mconcat . map (\a -> one (a, ()))
