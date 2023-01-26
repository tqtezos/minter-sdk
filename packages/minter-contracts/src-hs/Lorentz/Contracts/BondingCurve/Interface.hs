-- | Lorentz interface for the bonding curve contract
module Lorentz.Contracts.BondingCurve.Interface where

import Fmt (Buildable(..), genericF)
import Tezos.Address (detGenKeyAddress)

import Lorentz
import Lorentz.Contracts.MinterSdk (inBinFolder)
import Lorentz.Contracts.SimpleAdmin (AdminEntrypoints(..), AdminStorage(..))
import Lorentz.Contracts.Spec.FA2Interface (TokenId(..), TokenMetadata, mkTokenMetadata)
import qualified Lorentz.Contracts.FA2 as FA2 () -- TokenMetadata(..))

import Michelson.Text (unsafeMkMText)
import Michelson.Typed.Value (Value'(..))
import Michelson.Test.Import (importValue)


valueToLambda :: Value (ToT (Lambda a b)) -> Lambda a b
valueToLambda x =
  case x of
    VLam xs -> LorentzInstr xs

bondingCurveExampleFormula0Value :: IO (Value (ToT (Lambda Natural Mutez)))
bondingCurveExampleFormula0Value = importValue =<< inBinFolder "bonding_curve_example_formula_0.tz"

bondingCurveExampleFormula0Lambda :: IO (Lambda Natural Mutez)
bondingCurveExampleFormula0Lambda = valueToLambda <$> bondingCurveExampleFormula0Value


-- | "`calculateBasisPointFee` basisPoints amount" gives the expected basis point fee
calculateBasisPointFee :: Natural -> Integer -> Integer
calculateBasisPointFee basisPoints x =
  (fromIntegral basisPoints * x) `div` (100 * 100)

-- | Remove the basis point fee from the input and ensure that the subtraction
-- is safe, i.e. no negative results are returned.
--
-- If this result is non-negative, it's calculated as:
--
-- @
--   removeBasisPointFee basisPoints x = x - calculateBasisPointFee basisPoints x
-- @
--
-- otherwise an error is thrown.
removeBasisPointFee :: Natural -> Integer -> Integer
removeBasisPointFee basisPoints x =
  if 0 <= unsafeResult
    then unsafeResult
    else error $
      "removeBasisPointFee: basis point fee too large: (basisPoints, calculateBasisPointFee basisPoints x): " <>
      show (basisPoints, calculateBasisPointFee basisPoints x) <>
      " while x: " <>
      show x
  where
    unsafeResult = x - calculateBasisPointFee basisPoints x

-- | A piecewise polynomial is composed of a number of (length, coefficients
-- from x^0..) polynomials, ended by a single (coefficients from x^0..)
-- polynomial
data PiecewisePolynomial = PiecewisePolynomial
  { segments :: [(Natural, [Integer])]
  , last_segment :: [Integer]
  } deriving stock (Eq, Ord, Show)

customGeneric "PiecewisePolynomial" ligoCombLayout
deriving anyclass instance IsoValue PiecewisePolynomial
deriving anyclass instance HasAnnotation PiecewisePolynomial
instance Buildable PiecewisePolynomial where build = genericF

-- Run a polynomial [a0, a1, .. , an] on an input 'x' as
-- a0 * x^0 + a1 * x^1 + .. + an * x^n
runPolynomial :: [Integer] -> Integer -> Integer
runPolynomial coefficients x = sum . zipWith (*) coefficients $ iterate (* x) 1

-- Run a piecewise polynomial by finding the segment for the current offset and
-- calling runPolynomial
--
-- Given all of the piecewise_length's as a list piecewise_lengths, the current
-- segment can be considered the unique (n) for which the following holds:
--   sum (take n piecewise_lengths) <= x < sum (take (n+1) piecewise_lengths)
-- Or else the 'last_segment'
runPiecewisePolynomial :: PiecewisePolynomial -> Natural -> Integer
runPiecewisePolynomial PiecewisePolynomial{..} x = aux x segments
  where
    aux :: Natural -> [(Natural, [Integer])] -> Integer
    aux _offset [] = runPolynomial last_segment (toInteger x)
    aux  offset ((segmentLength, poly):segments') =
      -- TODO: remove comment
      -- if offset < segmentLength
      if offset <= segmentLength
         then runPolynomial poly (toInteger x)
         else aux (offset - segmentLength) segments'

polynomialToPiecewisePolynomial :: [Integer] -> PiecewisePolynomial
polynomialToPiecewisePolynomial polynomial = PiecewisePolynomial
  { segments = []
  , last_segment = polynomial
  }

-- | PiecewisePolynomial that always outputs constant
constantPiecewisePolynomial :: Integer -> PiecewisePolynomial
constantPiecewisePolynomial = polynomialToPiecewisePolynomial . (: [])

-- | PiecewisePolynomial that's always a line with formula: y(x) := y0 + slope * x
linearPiecewisePolynomial :: Integer -> Integer -> PiecewisePolynomial
linearPiecewisePolynomial y0 slope = polynomialToPiecewisePolynomial [y0, slope]

examplePiecewisePolynomial :: PiecewisePolynomial
examplePiecewisePolynomial = PiecewisePolynomial
  { segments = [(3, [0, 1])] -- f(x) = x | x < 3
  , last_segment = [0, 2]    -- f(x) = 2x
  }

examplePiecewisePolynomial' :: PiecewisePolynomial
examplePiecewisePolynomial' = PiecewisePolynomial
  { segments = [(6, [7, 8])]
  , last_segment = [4, 5]
  }


constantLambda :: Mutez -> Lambda Natural Mutez
constantLambda constant =
  Lorentz.drop #
  push constant

constantsLambda :: forall a. NiceConstant a => [a] -> Lambda Natural a
constantsLambda constants =
  left @Natural #
  push @[a] constants #
  Lorentz.swap #
  loopLeft
    ( push @Natural 1 #
      Lorentz.swap #
      sub #
      isNat #
      ifNone -- if None then Natural was 0 before subtracting 1 from it
        ( ifCons -- return value
            right
            ( push @MText (unsafeMkMText "list too short for index") #
              failWith
            )
        )
        ( Lorentz.swap # -- continue uncons-ing through list
          ifCons
            ( Lorentz.drop #
              Lorentz.swap #
              left @Natural
            )
            ( push @MText (unsafeMkMText "list too short for index") #
              failWith
            )
        )
    ) #
  Lorentz.swap #
  Lorentz.drop


data Storage c = Storage
  { admin :: AdminStorage
  , market_contract :: Address
  , auction_price :: Mutez
  , token_index :: Natural
  , token_metadata :: TokenMetadata
  , basis_points :: Natural
  , cost_mutez :: c
  , unclaimed :: Mutez
  } deriving stock (Eq, Show)

customGeneric "Storage" ligoCombLayout
deriving anyclass instance IsoValue c => IsoValue (Storage c)
deriving anyclass instance HasAnnotation c => HasAnnotation (Storage c)
instance Buildable c => Buildable (Storage c) where build = genericF

exampleAdminStorage :: AdminStorage
exampleAdminStorage = AdminStorage
  { admin = detGenKeyAddress "example-admin-key"
  , pendingAdmin = Nothing
  , paused = False
  }

exampleTokenMetadata :: TokenMetadata
exampleTokenMetadata = mkTokenMetadata symbol name decimals
  where
    symbol = "test_symbol"
    name = "This is a test! [name]"
    decimals = "12"

exampleStoragePiecewise :: Storage PiecewisePolynomial
exampleStoragePiecewise = Storage
  { admin = exampleAdminStorage
  , market_contract = detGenKeyAddress "dummy-impossible-contract-key"
  , auction_price = toMutez 0
  , token_index = 0
  , token_metadata = exampleTokenMetadata
  , basis_points = 100
  , cost_mutez = examplePiecewisePolynomial'
  , unclaimed = toMutez 0
  }

-- | exampleStorage with admin set
exampleStoragePiecewiseWithAdmin :: Address -> Storage PiecewisePolynomial
exampleStoragePiecewiseWithAdmin admin =
  exampleStoragePiecewise { admin = AdminStorage admin Nothing False }

exampleStorage :: Storage (Lambda Natural Mutez)
exampleStorage = Storage
  { admin = exampleAdminStorage
  , market_contract = detGenKeyAddress "dummy-impossible-contract-key"
  , auction_price = toMutez 0
  , token_index = 0
  , token_metadata = exampleTokenMetadata
  , basis_points = 100
  , cost_mutez = constantLambda (toEnum 0) -- examplePiecewisePolynomial'
  , unclaimed = toMutez 0
  }

-- | exampleStorage with admin set
exampleStorageWithAdmin :: Address -> Storage (Lambda Natural Mutez)
exampleStorageWithAdmin admin =
  exampleStorage { admin = AdminStorage admin Nothing False }

-- | exampleStoragePiecewise w/ distinct values
exampleStoragePiecewise' :: Storage PiecewisePolynomial
exampleStoragePiecewise' = Storage
  { admin = exampleAdminStorage
  , market_contract = detGenKeyAddress "dummy-impossible-contract-key"
  , auction_price = toMutez 0
  , token_index = 2
  , token_metadata = exampleTokenMetadata
  , basis_points = 100
  , cost_mutez = examplePiecewisePolynomial'
  , unclaimed = toMutez 3
  }

-- | Print properly-formatted michelson values for exampleStorage
--
-- ("admin","Pair (Pair \"tz2C97sask3WgSSg27bJhFxuBwW6MMU4uTPK\" False) None")
-- ("market_contract","\"tz2UXHa5WU79MnWF5uKFRM6qUowX13pdgJGy\"")
-- storage for distinguishing fields:
-- "{ Pair (Pair \"tz2C97sask3WgSSg27bJhFxuBwW6MMU4uTPK\" False) None; \"tz2UXHa5WU79MnWF5uKFRM6qUowX13pdgJGy\"; 0; 2; Pair 42 { Elt \"decimals\" 0x3132; Elt \"name\" 0x546869732069732061207465737421205b6e616d655d; Elt \"symbol\" 0x746573745f73796d626f6c }; 100; Pair { Pair 6 { 7; 8 } } { 4; 5 }; 3 }"
--
-- exampleStorage:
-- "{ Pair (Pair \"tz2C97sask3WgSSg27bJhFxuBwW6MMU4uTPK\" False) None; \"tz2UXHa5WU79MnWF5uKFRM6qUowX13pdgJGy\"; 0; 0; Pair 42 { Elt \"decimals\" 0x3132; Elt \"name\" 0x546869732069732061207465737421205b6e616d655d; Elt \"symbol\" 0x746573745f73796d626f6c }; 100; Pair { Pair 6 { 7; 8 } } { 4; 5 }; 0 }"printExampleStorage' :: IO ()
--
printExampleStoragePiecewise' :: IO ()
printExampleStoragePiecewise' = do
  print $ ("admin" :: String, printLorentzValue False exampleAdminStorage)
  print $ ("market_contract" :: String, printLorentzValue False $ market_contract exampleStoragePiecewise')
  putStrLn ("storage for distinguishing fields:" :: Text)
  print $ printLorentzValue False exampleStoragePiecewise'
  putStrLn ("" :: Text)
  putStrLn ("exampleStorage:" :: Text)
  print $ printLorentzValue False exampleStoragePiecewise

storageStr :: String
storageStr = "{ Pair (Pair \"tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb\" False) None; \"tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb\"; 0; 0; { Elt \"decimals\" 0x3132; Elt \"name\" 0x546869732069732061207465737421205b6e616d655d; Elt \"symbol\" 0x746573745f73796d626f6c }; 100; Pair { Pair 6 { 7; 8 } } { 4; 5 }; 3 }"



data Entrypoints
  = Default ()
  | Admin AdminEntrypoints
  | Set_delegate (Maybe KeyHash)
  | Withdraw ()
  | Buy ()
  | Buy_offchain Address
  | Sell TokenId
  | Sell_offchain (TokenId, Address)
  deriving stock (Eq, Show)

customGeneric "Entrypoints" ligoLayout
deriving anyclass instance IsoValue Entrypoints
deriving anyclass instance HasAnnotation Entrypoints

instance ParameterHasEntrypoints Entrypoints where
  -- EpdRecursive so that AdminEntrypoints are reached
  type ParameterEntrypointsDerivation Entrypoints = EpdRecursive

-- TODO: unused
-- -- | Error resulting from an empty formula
-- errEmptyFormula :: MText
-- errEmptyFormula = [mt|EMPTY_FORMULA|]

