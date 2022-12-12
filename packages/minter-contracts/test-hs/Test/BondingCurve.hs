{-# LANGUAGE InstanceSigs #-}

-- | Tests for bonding curve contract
module Test.BondingCurve where

import Prelude hiding (swap)

import Hedgehog ((===), Gen, Property, forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)

import Lorentz.Errors
import Lorentz.Value
import Michelson.Typed.Scope (ConstantScope)
import Michelson.Typed.Sing (KnownT)
import Morley.Nettest
import Morley.Nettest.Tasty (nettestScenarioCaps)

import Lorentz.Contracts.BondingCurve
import Lorentz.Contracts.BondingCurve.Interface
import Lorentz.Contracts.BondingCurve.Interface.Debug (DebugEntrypoints(..))
import Lorentz.Contracts.SimpleAdmin ()

import Test.Swaps.Util
import Test.Util

import Test.SimpleAdmin

originateBondingCurve
  :: MonadNettest caps base m
  => Storage
  -> m (ContractHandler Entrypoints Storage)
originateBondingCurve storage =
  originateSimple "bonding-curve" storage bondingCurveContract

originateDebugBondingCurve
  :: MonadNettest caps base m
  => Storage
  -> m (ContractHandler DebugEntrypoints Storage)
originateDebugBondingCurve storage =
  originateSimple "debug-bonding-curve" storage debugBondingCurveContract

-- Test SimpleAdmin admin ownership transfer
test_AdminChecks :: TestTree
test_AdminChecks =
  adminOwnershipTransferChecks @Entrypoints  @Storage
    (\admin ->
      originateBondingCurve
        (exampleStorageWithAdmin admin)
    )

-- TODO: include
-- test_Integrational :: TestTree
-- test_Integrational = testGroup "Integrational"
--   [
--     -- simple origination test
--     nettestScenarioCaps "Bonding curve origination" $ do
--       setup <- doFA2Setup
--       let admin ::< alice ::< SNil = sAddresses setup
--       let tokenId ::< SNil = sTokens setup
--       let bondingCurveStorage :: Storage = exampleStorage { admin = AdminStorage admin Nothing False }
--       bondingCurve <- originateBondingCurve bondingCurveStorage

--       return ()

--       -- TODO: enable
--       -- withSender admin $
--       --   -- call bondingCurve (Call @"Update_allowed") (mkAllowlistSimpleParam [fa2])
--       --   call bondingCurve (Call @"Buy") ()


------------------------------------------------------------------------------------------------------------------------
--       -- fa2 <- originateFA2 "fa2" setup [swap]

--       -- assertingBalanceDeltas fa2
--       --   [ (admin, tokenId) -: -3
--       --   , (alice, tokenId) -: 3
--       --   ] $ do
--       --     withSender admin $
--       --       call swap (Call @"Start") $ mkSingleOffer SwapOffer
--       --         { assetsOffered = [mkFA2Assets fa2 [(tokenId, 10)]]
--       --         , assetsRequested = [mkFA2Assets fa2 [(tokenId, 7)]]
--       --         }
--       --     withSender alice $
--       --       call swap (Call @"Accept") initSwapId
--   ]
------------------------------------------------------------------------------------------------------------------------



test_Debug :: TestTree
test_Debug = testGroup "Debug"
  [
    -- simple origination test
    nettestScenarioCaps "Bonding curve (debug) originate and call Cost with 4" $ do
      -- TODO test w/o FA2
      setup <- doFA2Setup @("addresses" :# 2) @("tokens" :# 0)
      let admin ::< _alice ::< SNil = sAddresses setup

      -- let tokenId ::< SNil = sTokens setup
      let bondingCurveStorage = exampleStorageWithAdmin admin
      bondingCurve <- originateDebugBondingCurve bondingCurveStorage

      -- TODO: enable
      -- withSender admin $
      --   -- call bondingCurve (Call @"Update_allowed") (mkAllowlistSimpleParam [fa2])
      --   call bondingCurve (Call @"Buy") ()

      call bondingCurve (Call @"Cost") (4 :: Natural)
        & expectError (WrappedValue (39 :: Integer))

  ]


data TestData = TestData
  -- | Polynomials have up to
  -- - 2^6=128 coefficients
  -- - 2^10=1024 coefficient absolute value
  -- - 2^9=512 offsets
  -- - 2^5=32 segments
  { piecewisePoly :: PiecewisePolynomial

  -- Tested up to 2^10=1024
  , polyInput :: Natural
  }
  deriving stock (Eq, Show)

-- | Shrink a list by alternatively removing any element
shrinkList :: [a] -> [[a]]
shrinkList xs = (\i -> take i xs ++ drop (i+1) xs) <$> [0..1 `subtract` length xs] -- this is length - 1, because (-) is overloaded weird by Lorentz

-- shrink towards 0 or keep equal (for shrinkPolynomial)
shrinkCoefficient :: Integer -> [Integer]
shrinkCoefficient x = [x - signum x, x]

-- cartesianProduct [[1,2],[3,4],[5,6]]
-- [[1,3,5],[1,3,6],[1,4,5],[1,4,6],[2,3,5],[2,3,6],[2,4,5],[2,4,6]]
cartesianProduct :: [[a]] -> [[a]]
cartesianProduct [] = [[]]
cartesianProduct (x:xs) = do
  y <- x
  ys <- cartesianProduct xs
  return (y:ys)

-- | all options of shrinking or now each coefficient
shrinkCoefficients :: [Integer] -> [[Integer]]
shrinkCoefficients xs = cartesianProduct $ fmap shrinkCoefficient xs

-- | Shrink list and/or coefficients
shrinkPolynomial :: [Integer] -> [[Integer]]
shrinkPolynomial xs = shrinkList xs >>= shrinkCoefficients

-- | Generate a polynomial
genPolynomial :: Gen [Integer]
genPolynomial =
  Gen.shrink shrinkList $
  Gen.list (Range.constant 0 128) (Gen.integral (Range.constant -1024 1024))

shrinkPiecewisePolySegment :: (Natural, [Integer]) -> [(Natural, [Integer])]
shrinkPiecewisePolySegment (segmentLength, polynomial) = do
  segmentLength' <- [segmentLength, 1 `subtract` segmentLength..0]
  polynomial' <- shrinkPolynomial polynomial
  pure (segmentLength', polynomial')

genPiecewisePolySegment :: Gen (Natural, [Integer])
genPiecewisePolySegment = Gen.shrink shrinkPiecewisePolySegment $ do
  segmentLength <- Gen.integral (Range.constant 0 32)
  polynomial <- genPolynomial
  pure (segmentLength, polynomial)

shrinkPiecewisePoly :: PiecewisePolynomial -> [PiecewisePolynomial]
shrinkPiecewisePoly PiecewisePolynomial{..} = do
  segments' <- shrinkList segments >>= cartesianProduct . fmap shrinkPiecewisePolySegment

  last_segment' <- shrinkPolynomial last_segment
  pure $ PiecewisePolynomial
    { segments = segments'
    , last_segment = last_segment'
    }

genPiecewisePoly :: Gen PiecewisePolynomial
genPiecewisePoly = Gen.shrink shrinkPiecewisePoly $ do
  segments <- Gen.shrink shrinkList $
    Gen.list (Range.constant 0 32) genPiecewisePolySegment
  last_segment <- genPolynomial
  pure $ PiecewisePolynomial
    { segments = segments
    , last_segment = last_segment
    }

shrinkTestData :: TestData -> [TestData]
shrinkTestData TestData{..} = do
  piecewisePoly' <- shrinkPiecewisePoly piecewisePoly
  polyInput' <- [polyInput, 1 `subtract` polyInput..0]
  pure $ TestData
    { piecewisePoly = piecewisePoly'
    , polyInput = polyInput'
    }

genTestData :: Gen TestData
genTestData = Gen.shrink shrinkTestData $ do
  piecewisePoly <- genPiecewisePoly
  polyInput <- Gen.integral (Range.constant 0 1024)
  pure $ TestData
    { piecewisePoly = piecewisePoly
    , polyInput = polyInput
    }

-- -- | A piecewise polynomial is composed of a number of (length, coefficients
-- -- from x^0..) polynomials, ended by a single (coefficients from x^0..)
-- -- polynomial
-- data PiecewisePolynomial = PiecewisePolynomial
--   { segments :: [(Natural, [Integer])]
--   , last_segment :: [Integer]
--   } deriving stock (Eq, Ord, Show)

-- runPolynomial behaves as expected for:
-- f(x) = 1
hprop_runPolynomial_constant :: Property
hprop_runPolynomial_constant = property $ do
  x <- forAll $ Gen.integral (Range.constant (negate 1024) 1024)
  runPolynomial [1] x === 1

-- runPolynomial behaves as expected for:
-- f(x) = x
hprop_runPolynomial_line :: Property
hprop_runPolynomial_line = property $ do
  x <- forAll $ Gen.integral (Range.constant (negate 1024) 1024)
  runPolynomial [0, 1] x === x

-- runPolynomial behaves as expected for:
-- f(x) = 2 x^2 + 3 x - 5
hprop_runPolynomial_quadratic :: Property
hprop_runPolynomial_quadratic = property $ do
  x <- forAll $ Gen.integral (Range.constant (negate 1024) 1024)
  runPolynomial [-5, 3, 2] x === 2 * x^2 + 3 * x - 5

-- runPiecewisePolynomial is equivalent to runPolynomial when there's only a
-- last_segment
hprop_runPiecewisePolynomial_is_runPolynomial :: Property
hprop_runPiecewisePolynomial_is_runPolynomial = property $ do
  TestData{piecewisePoly, polyInput} <- forAll genTestData
  let polynomial = last_segment piecewisePoly

  runPolynomial polynomial (toInteger polyInput) ===
    runPiecewisePolynomial (PiecewisePolynomial
      { segments = []
      , last_segment = polynomial
      }) polyInput

-- runPiecewisePolynomial is equivalent to runPolynomial when the input is
-- >= sum segmentLength's
hprop_runPiecewisePolynomial_is_runPolynomial_after_offsets :: Property
hprop_runPiecewisePolynomial_is_runPolynomial_after_offsets = property $ do
  TestData{piecewisePoly, polyInput} <- forAll genTestData
  let polynomial = last_segment piecewisePoly
  let offsetInput :: Natural = polyInput + sum (fmap fst (segments piecewisePoly))

  runPolynomial polynomial (toInteger offsetInput) ===
    runPiecewisePolynomial piecewisePoly offsetInput



-- runPiecewisePolynomial can implement
-- abs (x - abs constant)
hprop_runPiecewisePolynomial_abs :: Property
hprop_runPiecewisePolynomial_abs = property $ do
  let genNatUpTo2ToThe20 = Gen.integral $ Range.constant 0 (2^20)
  (offset, x) <- forAll $ liftA2 (,) genNatUpTo2ToThe20 genNatUpTo2ToThe20
  toInteger (abs (x - offset)) ===
    runPiecewisePolynomial (PiecewisePolynomial
      { segments = [(offset + 1, [toInteger offset, -1])] -- if x < offset + 1 == x <= offset then -x
      , last_segment = [0, 1]                             -- else x
      }) x

-- | Call the "Cost" entrypoint on the debugBondingCurveContract to check the
-- LIGO implementation of runPiecewisePolynomial against the Haskell one
hprop_piecewise_polynomial_correct :: Property
hprop_piecewise_polynomial_correct =
  property $ do
    TestData{piecewisePoly, polyInput} <- forAll genTestData
    clevelandProp $ do
      -- TODO: test w/o FA2 or using NFT contract
      setup <- doFA2Setup @("addresses" :# 1) @("tokens" :# 0)

      let alice ::< SNil = sAddresses setup
      let bondingCurveStorage = (exampleStorageWithAdmin alice) { cost_mutez = piecewisePoly }
      bondingCurve <- originateDebugBondingCurve bondingCurveStorage

      call bondingCurve (Call @"Cost") polyInput
        & expectError (WrappedValue (runPiecewisePolynomial piecewisePoly polyInput))



-- TODO: relocate, used for catching failWith (_ :: int)
-------------------------------------------------------------------------------------------
-- BEGIN WrappedValue
-------------------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------------------
-- END WrappedValue
-------------------------------------------------------------------------------------------

