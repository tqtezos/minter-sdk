{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Test.Util
  ( (-:)
  , type (:#)
  , pattern (::<)
  , pattern SNil
  , assertingBalanceDeltas

    -- Re-exports
  , Sized
  ) where

import Data.Kind (Type)
import Data.Sized (Sized)
import qualified Data.Sized as Sized
import Data.Type.Natural.Lemma.Order (type (<))
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
