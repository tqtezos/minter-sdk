-- | Property tests for bonding curve contract
module Test.BondingCurve.Property where

import Prelude hiding (swap)

import Hedgehog ((===), Gen, MonadTest, Property, forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
-- import Test.Tasty (TestTree, testGroup)

-- import Lorentz.Errors
-- import Lorentz.Value
-- import Michelson.Typed.Scope (ConstantScope)
-- import Michelson.Typed.Sing (KnownT)
import Morley.Nettest
-- import Morley.Nettest.Tasty (nettestScenarioCaps)

-- import Lorentz.Contracts.BondingCurve
import Lorentz.Contracts.BondingCurve.Interface
-- import Lorentz.Contracts.BondingCurve.Interface.Debug (DebugEntrypoints(..))
-- import Lorentz.Contracts.MinterCollection.Nft.Types

-- import Lorentz.Contracts.SimpleAdmin

-- import Test.Swaps.Util
import Test.Util

import Test.BondingCurve (originateDebugBondingCurve)
-- import Test.SimpleAdmin
-- import Test.MinterCollection.Nft (originateNft)


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

testDataSizes :: TestData -> (Int, Int, Int)
testDataSizes TestData{..} =
  ( length (segments piecewisePoly)
  , safeMaximum $ fmap (length . snd) (segments piecewisePoly)
  , length (last_segment piecewisePoly)
  )
  where
    -- maximum fails on []
    safeMaximum :: [Int] -> Int
    safeMaximum [] = 0
    safeMaximum xs = maximum xs

-- -- | A piecewise polynomial is composed of a number of (length, coefficients
-- -- from x^0..) polynomials, ended by a single (coefficients from x^0..)
-- -- polynomial
-- data PiecewisePolynomial = PiecewisePolynomial
--   { segments :: [(Natural, [Integer])]
--   , last_segment :: [Integer]
--   } deriving stock (Eq, Ord, Show)



-- | Shrink a list by alternatively removing any element
shrinkList :: [a] -> [[a]]
shrinkList xs = (\i -> take i xs ++ drop (i+1) xs) <$> [0..1 `subtract` length xs] -- this is length - 1, because (-) is overloaded weird by Lorentz

-- | Shrink a list by alternatively removing any element, except the last one
shrinkListNonEmpty :: [a] -> [[a]]
shrinkListNonEmpty [] = []
shrinkListNonEmpty [_] = []
shrinkListNonEmpty xs = (\i -> take i xs ++ drop (i+1) xs) <$> [0..1 `subtract` length xs] -- this is length - 1, because (-) is overloaded weird by Lorentz

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
-- shrinkPolynomial xs = shrinkList xs >>= shrinkCoefficients
shrinkPolynomial xs = shrinkListNonEmpty xs >>= shrinkCoefficients

-- TODO: generates non-empty polynomial's
-- | Generate a polynomial
genPolynomial :: Gen [Integer]
genPolynomial =
  -- Gen.shrink shrinkList $
  Gen.shrink shrinkListNonEmpty $
  -- Gen.list (Range.constant 0 32) (Gen.integral (Range.constant -1024 1024))
  Gen.list (Range.constant 1 32) (Gen.integral (Range.constant -1024 1024))

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
    Gen.list (Range.constant 0 16) genPiecewisePolySegment
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

-- | TestData where runPiecewisePolynomial piecewisePoly polyInput >= 0
genNonNegativeTestData :: Gen TestData
genNonNegativeTestData =
  Gen.filter
    (\TestData{..} -> 0 <= runPiecewisePolynomial piecewisePoly polyInput)
    genTestData



-- TODO: re-enable!

-- -- runPolynomial behaves as expected for:
-- -- f(x) = 1
-- hprop_runPolynomial_constant :: Property
-- hprop_runPolynomial_constant = property $ do
--   x <- forAll $ Gen.integral (Range.constant (negate 1024) 1024)
--   runPolynomial [1] x === 1

-- -- runPolynomial behaves as expected for:
-- -- f(x) = x
-- hprop_runPolynomial_line :: Property
-- hprop_runPolynomial_line = property $ do
--   x <- forAll $ Gen.integral (Range.constant (negate 1024) 1024)
--   runPolynomial [0, 1] x === x

-- -- runPolynomial behaves as expected for:
-- -- f(x) = 2 x^2 + 3 x - 5
-- hprop_runPolynomial_quadratic :: Property
-- hprop_runPolynomial_quadratic = property $ do
--   x :: Integer <- forAll $ Gen.integral (Range.constant (negate 1024) 1024)
--   runPolynomial [-5, 3, 2] x === 2 * x^(2 :: Integer) + 3 * x - 5

-- -- runPiecewisePolynomial (constantPiecewisePolynomial x) == x
-- hprop_runPiecewisePolynomial_constant :: Property
-- hprop_runPiecewisePolynomial_constant = property $ do
--   TestData{piecewisePoly, polyInput} <- forAll genTestData
--   let constant' = maybe 0 fst $ uncons $ last_segment piecewisePoly
--   runPiecewisePolynomial (constantPiecewisePolynomial constant') polyInput ===
--     constant'

-- -- runPiecewisePolynomial is equivalent to runPolynomial when there's only a
-- -- last_segment
-- hprop_runPiecewisePolynomial_is_runPolynomial :: Property
-- hprop_runPiecewisePolynomial_is_runPolynomial = property $ do
--   TestData{piecewisePoly, polyInput} <- forAll genTestData
--   let polynomial = last_segment piecewisePoly

--   runPolynomial polynomial (toInteger polyInput) ===
--     runPiecewisePolynomial (polynomialToPiecewisePolynomial polynomial) polyInput

-- -- runPiecewisePolynomial is equivalent to runPolynomial when the input is
-- -- >= sum segmentLength's
-- hprop_runPiecewisePolynomial_is_runPolynomial_after_offsets :: Property
-- hprop_runPiecewisePolynomial_is_runPolynomial_after_offsets = property $ do
--   TestData{piecewisePoly, polyInput} <- forAll genTestData
--   let polynomial = last_segment piecewisePoly
--   let offsetInput :: Natural = polyInput + sum (fmap fst (segments piecewisePoly))

--   runPolynomial polynomial (toInteger offsetInput) ===
--     runPiecewisePolynomial piecewisePoly offsetInput

-- -- TODO: fix this test!
-- -- runPiecewisePolynomial can implement
-- -- abs (x - abs constant)
-- hprop_runPiecewisePolynomial_abs :: Property
-- hprop_runPiecewisePolynomial_abs = property $ do
--   () === ()

--   -- let genNatUpTo2ToThe20 = Gen.integral $ Range.constant 0 (2^(20 :: Integer))
--   -- (offset, x) <- forAll $ liftA2 (,) genNatUpTo2ToThe20 genNatUpTo2ToThe20
--   -- abs (x - offset) ===
--   --   runPiecewisePolynomial (PiecewisePolynomial
--   --     { segments = [(fromInteger offset + 1, [offset, -1])] -- if x < offset + 1 == x <= offset then -x
--   --     , last_segment = [0, 1]                               -- else x
--   --     }) (fromInteger x)


-- Call the "Cost" entrypoint on the debugBondingCurveContract to check the
-- LIGO implementation of runPiecewisePolynomial against the Haskell one
--
-- (Run only on polynomials producing non-negative output for the given input,
-- see genNonNegativeTestData)
hprop_piecewise_polynomial_correct :: Property
hprop_piecewise_polynomial_correct =
  property $ do
    testData <- forAll genTestData
    testPiecewisePolynomialUsingCost testData

failingTestData :: TestData
failingTestData = TestData
  { piecewisePoly =
      PiecewisePolynomial
        { segments =
            [ ( 24 , [ -1024 ] )
            , ( 18 , [ -1024 ] )
            , ( 32 , [ -1024 ] )
            , ( 2 , [ -1024 ] )
            , ( 15 , [ -1024 ] )
            , ( 18 , [ -1024 ] )
            , ( 3 , [ -1024 ] )
            , ( 15 , [ -1024 ] )
            , ( 12 , [ -1024 ] )
            , ( 16 , [ -1024 ] )
            , ( 13 , [ -1024 ] )
            , ( 26 , [ -1024 ] )
            ]
        , last_segment = [ -1023 ]
        }
  , polyInput = 194
  }

hprop_piecewise_polynomial_correct_failing :: Property
hprop_piecewise_polynomial_correct_failing =
  property $ do
    testPiecewisePolynomialUsingCost failingTestData

testPiecewisePolynomialUsingCost :: (MonadIO m, MonadTest m) => TestData -> m ()
testPiecewisePolynomialUsingCost TestData{piecewisePoly, polyInput} =
  clevelandProp $ do
    setup <- doFA2Setup @("addresses" :# 1) @("tokens" :# 0)
    let alice ::< SNil = sAddresses setup
    let bondingCurveStorage = (exampleStorageWithAdmin alice) { cost_mutez = piecewisePoly }
    bondingCurve <- originateDebugBondingCurve bondingCurveStorage
    let expectedCost = runPiecewisePolynomial piecewisePoly polyInput
    call bondingCurve (Call @"Cost") polyInput
      & expectError (WrappedValue expectedCost)


