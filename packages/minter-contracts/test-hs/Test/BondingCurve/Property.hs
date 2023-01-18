{-# LANGUAGE OverloadedLists #-}

-- | Property tests for bonding curve contract
module Test.BondingCurve.Property where

import Fmt (Buildable, Builder, build, unlinesF)

import Prelude hiding (swap)

import Hedgehog ((===), Gen, MonadTest, Property, PropertyT, forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Gen.QuickCheck as Gen
import qualified Hedgehog.Range as Range
import Test.QuickCheck (NonEmptyList(..), NonNegative(..))

import Lorentz.Contracts.Spec.FA2Interface
import Lorentz.Value (Mutez, ToAddress(..), toMutez)
import Michelson.Text (unsafeMkMText)
import Morley.Nettest
import Morley.Nettest.Abstract (SpecificOrDefaultAliasHint(..))

import Lorentz.Contracts.BondingCurve.Interface
import Lorentz.Contracts.MinterCollection.Nft.Types

import Test.Util

import Test.BondingCurve
import Test.MinterCollection.Nft (originateNft)

-- TestData in a format where we get Arbitrary for free
--
-- data ValidTestData = ValidTestData
--   { piecewisePoly :: ([(NonNegative Integer, NonEmptyList Integer)], NonEmptyList Integer)
--   , polyInput :: NonNegative Integer
--   }
--   deriving stock (Eq, Show, Generic)
--
-- instance Arbitrary ValidTestData where
--   arbitrary = liftM2 ValidTestData arbitrary arbitrary
--   shrink = recursivelyShrink
type ValidTestData = (([(NonNegative Integer, NonEmptyList Integer)], NonEmptyList Integer), NonNegative Integer)

-- convert ValidTestData to TestData
fromValidTestData :: ValidTestData -> TestData
fromValidTestData (piecewisePoly, polyInput) = TestData
  { piecewisePoly =
      uncurry PiecewisePolynomial .
      bimap
        (fmap (bimap (fromInteger . getNonNegative) getNonEmpty))
        getNonEmpty $
      piecewisePoly
  , polyInput = fromInteger $ getNonNegative polyInput
  }

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

-- drop all segments except last_segment
testDataWithOnlyLastSegment :: TestData -> TestData
testDataWithOnlyLastSegment TestData{..} = TestData
  { piecewisePoly = PiecewisePolynomial
      { segments = []
      , last_segment = last_segment piecewisePoly
      }
  , polyInput = polyInput
  }

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

-- | Generate a polynomial
genPolynomial :: Gen [Integer]
genPolynomial =
  -- Gen.shrink shrinkList $
  Gen.shrink shrinkListNonEmpty $
  -- Gen.list (Range.constant 0 32) (Gen.integral (Range.constant -1024 1024))
  Gen.list (Range.constant 1 32) (Gen.integral (Range.constant -1024 1024))

shrinkPiecewisePolySegment :: (Natural, [Integer]) -> [(Natural, [Integer])]
shrinkPiecewisePolySegment (segmentLength, polynomial) = do
  segmentLength' <- [segmentLength, safePred segmentLength..0]
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
  polyInput' <- [polyInput, safePred polyInput..0]
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
  x :: Integer <- forAll $ Gen.integral (Range.constant (negate 1024) 1024)
  runPolynomial [-5, 3, 2] x === 2 * x^(2 :: Integer) + 3 * x - 5

-- runPiecewisePolynomial (constantPiecewisePolynomial x) _ == x
hprop_runPiecewisePolynomial_constant :: Property
hprop_runPiecewisePolynomial_constant = property $ do
  TestData{piecewisePoly, polyInput} <- forAll genTestData
  let constant' = maybe 0 fst $ uncons $ last_segment piecewisePoly
  runPiecewisePolynomial (constantPiecewisePolynomial constant') polyInput ===
    constant'

-- runPiecewisePolynomial (linearPiecewisePolynomial rise run) x == rise + run * x
hprop_runPiecewisePolynomial_linear :: Property
hprop_runPiecewisePolynomial_linear = property $ do
  TestData{piecewisePoly, polyInput} <- forAll genTestData
  let (rise, run) = case last_segment piecewisePoly of
                      [] -> (0, 0)
                      [x] -> (x, 0)
                      (x:y:_) -> (x, y)
  runPiecewisePolynomial (linearPiecewisePolynomial rise run) polyInput ===
    rise + run * toInteger polyInput

-- runPiecewisePolynomial is equivalent to runPolynomial when there's only a
-- last_segment
hprop_runPiecewisePolynomial_is_runPolynomial :: Property
hprop_runPiecewisePolynomial_is_runPolynomial = property $ do
  TestData{piecewisePoly, polyInput} <- forAll genTestData
  let polynomial = last_segment piecewisePoly

  runPolynomial polynomial (toInteger polyInput) ===
    runPiecewisePolynomial (polynomialToPiecewisePolynomial polynomial) polyInput

-- runPiecewisePolynomial is equivalent to runPolynomial when the input is
-- >= sum segmentLength's
hprop_runPiecewisePolynomial_is_runPolynomial_after_offsets :: Property
hprop_runPiecewisePolynomial_is_runPolynomial_after_offsets = property $ do
  TestData{piecewisePoly, polyInput} <- forAll genTestData
  let polynomial = last_segment piecewisePoly
  let offsetInput :: Natural = polyInput + sum (fmap fst (segments piecewisePoly))

  runPolynomial polynomial (toInteger offsetInput) ===
    runPiecewisePolynomial piecewisePoly offsetInput


-- Assert that calling the "Cost" entrypoint matches the implementation of runPiecewisePolynomial
testPiecewisePolynomialUsingCost :: (MonadIO m, MonadTest m) => TestData -> m ()
testPiecewisePolynomialUsingCost TestData{piecewisePoly, polyInput} =
  clevelandProp $ do
    setup <- doFA2Setup @("addresses" :# 1) @("tokens" :# 0)
    let admin ::< SNil = sAddresses setup
    let bondingCurveStorage = (exampleStorageWithAdmin admin) { cost_mutez = piecewisePoly }
    bondingCurve <- originateDebugBondingCurvePiecewise bondingCurveStorage
    let expectedCost = runPiecewisePolynomial piecewisePoly polyInput
    call bondingCurve (Call @"Cost") polyInput
      & expectError (WrappedValue expectedCost)

-- Call the "Cost" entrypoint on the debugBondingCurveContract to check the
-- LIGO implementation of runPiecewisePolynomial against the Haskell one
--
-- (Run only on polynomials producing non-negative output for the given input,
-- see genNonNegativeTestData)
hprop_piecewise_polynomial_correct :: Property
hprop_piecewise_polynomial_correct =
  property $ do
    testData <- fromValidTestData <$> forAll Gen.arbitrary
    testPiecewisePolynomialUsingCost testData

-- hprop_piecewise_polynomial_correct but single-section piecewise polynomials
hprop_piecewise_polynomial_correct_only_last_segment :: Property
hprop_piecewise_polynomial_correct_only_last_segment =
  property $ do
    testData <- testDataWithOnlyLastSegment . fromValidTestData <$> forAll Gen.arbitrary
    testPiecewisePolynomialUsingCost testData

-- off-by-1 error
-- 194 == 24 + 18 + 32 + 2 + 15 + 18 + 3 + 15 + 12 + 16 + 13 + 26
unitTestData :: TestData
unitTestData = TestData
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

unitTestData2 :: TestData
unitTestData2 = TestData
  { piecewisePoly =
      PiecewisePolynomial
        { segments =
            [ ( 1 , [ 0 ] )
            , ( 1 , [ 0 ] )
            ]
        , last_segment = [ 1 ]
        }
  , polyInput = 2
  }

unitTestData3 :: TestData
unitTestData3 = TestData
  { piecewisePoly =
      PiecewisePolynomial
        { segments =
            [ ( 1 , [ 0 ] )
            ]
        , last_segment = [ 1 ]
        }
  , polyInput = 1
  }

hprop_piecewise_polynomial_correct_unit :: Property
hprop_piecewise_polynomial_correct_unit =
  property $ do
    testPiecewisePolynomialUsingCost unitTestData
    testPiecewisePolynomialUsingCost unitTestData2
    testPiecewisePolynomialUsingCost unitTestData3



-- Assert that calling the "Pow" entrypoint matches the implementation of (^) for natural numbers
hprop_Pow :: Property -- (MonadIO m, MonadTest m) => m ()
hprop_Pow =
  property $ do
    x <- fromIntegral . getNonNegative @Integer <$> forAll Gen.arbitrary
    n <- fromIntegral . getNonNegative @Integer <$> forAll Gen.arbitrary

    clevelandProp $ do
      setup <- doFA2Setup @("addresses" :# 1) @("tokens" :# 0)
      let admin ::< SNil = sAddresses setup
      let bondingCurveStorage = exampleStoragePiecewiseWithAdmin admin
      bondingCurve <- originateDebugBondingCurvePiecewise bondingCurveStorage
      call bondingCurve (Call @"Pow") (x, n)
        & expectError (WrappedValue (x ^ n))


-- Assert that calling the "Pow" entrypoint matches the implementation of (^) for natural numbers
hprop_ExampleFormula0 :: Property
hprop_ExampleFormula0 =
  property $ do
    x <- fromIntegral . getNonNegative @Integer <$> forAll Gen.arbitrary
    x' <- (+ 30000) . fromIntegral . getNonNegative @Integer <$> forAll Gen.arbitrary

    clevelandProp $ do
      setup <- doFA2Setup @("addresses" :# 1) @("tokens" :# 0)
      let admin ::< SNil = sAddresses setup
      let bondingCurveStorage = exampleStoragePiecewiseWithAdmin admin
      bondingCurve <- originateDebugBondingCurvePiecewise bondingCurveStorage

      let n = id @Natural
      let exampleFormula0 :: Natural -> Mutez = \y ->
            if y < 30000
               then fromIntegral $ y `div` n 3000
               else fromIntegral $ 10 * (n 1001^y `div` n 1000^y)

      call bondingCurve (Call @"ExampleFormula0") x
        & expectError (WrappedValue (exampleFormula0 x))

      call bondingCurve (Call @"ExampleFormula0") x'
        & expectError (WrappedValue (exampleFormula0 x'))


-- Assert that calling the "Pow" entrypoint matches the implementation of (^) for natural numbers
hprop_ExampleFormula0_lambda :: Property
hprop_ExampleFormula0_lambda =
  property $ do
    x <- fromIntegral . getNonNegative @Integer <$> forAll Gen.arbitrary

    exampleFormula0Lambda <- liftIO bondingCurveExampleFormula0Lambda

    clevelandProp $ do
      setup <- doFA2Setup @("addresses" :# 1) @("tokens" :# 0)
      let admin ::< SNil = sAddresses setup
      let bondingCurveStorage = (exampleStorageWithAdmin admin) { cost_mutez = exampleFormula0Lambda }
      bondingCurve <- originateDebugBondingCurve bondingCurveStorage

      let n = id @Natural
      let exampleFormula0 :: Natural -> Mutez = \y ->
            if y < 30000
               then fromIntegral $ y `div` n 3000
               else fromIntegral $ 10 * (n 1001^y `div` n 1000^y)

      call bondingCurve (Call @"Cost") x
        & expectError (WrappedValue (exampleFormula0 x))



-- safePred n = n - 1, but never underflows
safePred :: Natural -> Natural
safePred 0 = 0
safePred n = pred n

testDataSmallEnoughForMutez :: MonadIO m => PropertyT m (Mutez, Natural, TestData, [(Integer, Integer)], Integer, Integer)
testDataSmallEnoughForMutez = do
  testData@TestData{..}  <- return $ fromValidTestData $
    ( ( []
      , NonEmpty [100, 200, 300, 400, 500, 600]
      )
    , NonNegative 0)

  let numBuyers = polyInput
  let auctionPrice = 10
  let basisPoints = 1

  let expectedCosts =
        [ fromIntegral auctionPrice + runPiecewisePolynomial piecewisePoly buyer
        | buyer <- [0..safePred numBuyers]
        ]
  let expectedCostsWithBasisPoints =
        [ cost + calculateBasisPointFee basisPoints cost
        | cost <- expectedCosts
        ]
  let expectedProfit = sum
        [ calculateBasisPointFee basisPoints cost
        | cost <- expectedCosts
        ]
  let expectedTotalCostWithFees = sum expectedCostsWithBasisPoints

  if expectedTotalCostWithFees <= fromIntegral (maxBound :: Mutez)
     then return
            ( auctionPrice
            , basisPoints
            , testData
            , zip expectedCosts expectedCostsWithBasisPoints
            , expectedTotalCostWithFees
            , expectedProfit)
     else do
       fail $ "too big: " <> show (auctionPrice, basisPoints, testData)


(@<=) :: (HasCallStack, MonadNettest caps base m, Buildable a, Ord a) => a -> a -> m ()
(@<=) x y = do
  assert (x <= y) $
    unlinesF
      ([ "Not <= as asserted:"
      , build x
      , ">"
      , build y
      ] :: [Builder])


-- buy many tokens, sell all of them, ensure costs and basis_points as expected
hprop_batch_buy_sell :: Property
hprop_batch_buy_sell =
  property $ do
    (auctionPrice
      , basisPoints
      , TestData{piecewisePoly}
      , expectedCostsWithBasisPoints
      , expectedTotalCostWithFees
      , expectedProfit) <- testDataSmallEnoughForMutez

    clevelandProp $ do
      setup <- doFA2Setup @("addresses" :# 1) @("tokens" :# 0)
      let admin ::< SNil = sAddresses setup
      nft <- originateNft ((exampleNftStorageWithAdmin admin)
        { assets = exampleNftTokenStorage { ledger = [(TokenId 0, admin)]
                                          , next_token_id = TokenId 1
                                          } })
      let bondingCurveStorage :: Storage PiecewisePolynomial =
            (exampleStorageWithAdmin admin)
              { auction_price = auctionPrice
              , market_contract = toAddress nft
              , cost_mutez = piecewisePoly
              , basis_points = basisPoints
              }
      bondingCurve <- originateBondingCurvePiecewise bondingCurveStorage

      -- admin needs to set operator on (TokenId 0) to allow bondingCurve to mint
      withSender admin $
        call nft (Call @"Update_operators")
          [ AddOperator OperatorParam
              { opOwner = admin
              , opOperator = toAddress bondingCurve
              , opTokenId = TokenId 0
              }
          ]

      let defaultBalance :: Mutez = 900000
      getBalance admin @@== defaultBalance

      -- fill admin balance using fresh address creation
      replicateM_ (fromInteger (expectedTotalCostWithFees `div` fromIntegral defaultBalance)) $ do
        mutezFiller <- newAddress DefaultAliasHint

        adminBalanceBeforeFill <- getBalance admin

        withSender mutezFiller $
          getBalance mutezFiller >>= transferMoney admin

        -- ensure transferred
        adminBalanceAfterFill <- getBalance admin
        (adminBalanceAfterFill - adminBalanceBeforeFill) @== defaultBalance


      adminBalance <- getBalance admin
      toMutez (fromIntegral expectedTotalCostWithFees) @<= adminBalance

      -- each buyer buys 1 token
      let indexedExpectedCostsWithBasisPoints = zip [1..] expectedCostsWithBasisPoints
      buyersAndCosts <- forM indexedExpectedCostsWithBasisPoints $ \(tokenIndex, (expectedCost, expectedCostWithBasisPoints)) -> do
        buyer <- newAddress "buyer"

        -- admin fills up buyer's wallet if 0 < cost
        if 0 < expectedCostWithBasisPoints
           then withSender admin $
                  transferMoney buyer (fromIntegral expectedCostWithBasisPoints)
           else return ()

        buyerBalanceBefore <- getBalance buyer

        -- buy one token
        withSender buyer $
          transfer $
            TransferData
              { tdTo = bondingCurve
              , tdAmount = fromIntegral expectedCostWithBasisPoints
              , tdEntrypoint = ep "buy"
              , tdParameter = ()
              }

        buyerBalanceAfter <- getBalance buyer

        -- ensure cost was expected
        (buyerBalanceBefore - buyerBalanceAfter) @== fromIntegral expectedCostWithBasisPoints

        return (tokenIndex, buyer, expectedCost)

      forM_ (reverse buyersAndCosts) $ \(tokenIndex, seller, expectedCost) -> do
        -- seller needs to set operator to sell
        withSender seller $
          call nft (Call @"Update_operators")
            [ AddOperator OperatorParam
                { opOwner = seller
                , opOperator = toAddress bondingCurve
                , opTokenId = TokenId tokenIndex
                }
            ]

        sellerBalanceBefore <- getBalance seller

        -- sell one token
        withSender seller $
          call bondingCurve (Call @"Sell") (TokenId tokenIndex)

        -- ensure cost was expected
        sellerBalanceAfter <- getBalance seller
        (sellerBalanceAfter - sellerBalanceBefore) @== fromIntegral expectedCost

      -- ensure zero tokens remaining
      preBuyStorage <- getStorage' bondingCurve
      preBuyStorage @== bondingCurveStorage

      if expectedProfit == 0
        then do
          -- nothing to withdraw
          withSender admin $
            call bondingCurve (Call @"Withdraw") ()
              & expectError (unsafeMkMText "UNCLAIMED=0")
        else do
           adminBalanceBefore <- getBalance admin

           -- ensure sum of basis_points fees can be withdrawn
           withSender admin $
             call bondingCurve (Call @"Withdraw") ()

           adminBalanceAfter <- getBalance admin
           (adminBalanceBefore - adminBalanceAfter) @== fromIntegral expectedProfit

