-- | Lorentz interface for the bonding curve contract
module Lorentz.Contracts.BondingCurve.Interface where

import Fmt (Buildable(..), genericF)
import Lorentz
import Tezos.Address (detGenKeyAddress)

import Lorentz.Contracts.SimpleAdmin (AdminEntrypoints(..), AdminStorage(..))
import qualified Lorentz.Contracts.FA2 as FA2 (TokenMetadata(..))
import Lorentz.Contracts.Spec.FA2Interface (TokenId(..), mkTokenMetadata)

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
      if offset < segmentLength
         then runPolynomial poly (toInteger x)
         else aux (offset - segmentLength) segments'

polynomialToPiecewisePolynomial :: [Integer] -> PiecewisePolynomial
polynomialToPiecewisePolynomial polynomial = PiecewisePolynomial
  { segments = []
  , last_segment = polynomial
  }

constantPiecewisePolynomial :: Integer -> PiecewisePolynomial
constantPiecewisePolynomial = polynomialToPiecewisePolynomial . (: [])

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

data Storage = Storage
  { admin :: AdminStorage
  , market_contract :: Address
  , auction_price :: Mutez
  , token_index :: Natural
  , token_metadata :: FA2.TokenMetadata
  , basis_points :: Natural
  , cost_mutez :: PiecewisePolynomial
  , unclaimed :: Mutez
  } deriving stock (Eq, Show)

customGeneric "Storage" ligoCombLayout
deriving anyclass instance IsoValue Storage
deriving anyclass instance HasAnnotation Storage
instance Buildable Storage where build = genericF

exampleAdminStorage :: AdminStorage
exampleAdminStorage = AdminStorage
  { admin = detGenKeyAddress "example-admin-key"
  , pendingAdmin = Nothing
  , paused = False
  }

exampleTokenMetadata :: FA2.TokenMetadata
exampleTokenMetadata = FA2.TokenMetadata
  { tokenId = TokenId 42 -- :: FA2I.TokenId
  , tokenInfo = mkTokenMetadata symbol name decimals -- :: FA2I.TokenMetadata
  }
  where
    symbol = "test_symbol"
    name = "This is a test! [name]"
    decimals = "12"

exampleStorage :: Storage
exampleStorage = Storage
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
exampleStorageWithAdmin :: Address -> Storage
exampleStorageWithAdmin admin =
  exampleStorage { admin = AdminStorage admin Nothing False }

-- | exampleStorage w/ distinct values
exampleStorage' :: Storage
exampleStorage' = Storage
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
printExampleStorage' :: IO ()
printExampleStorage' = do
  print $ ("admin" :: String, printLorentzValue False exampleAdminStorage)
  print $ ("market_contract" :: String, printLorentzValue False $ market_contract exampleStorage')
  putStrLn ("storage for distinguishing fields:" :: Text)
  print $ printLorentzValue False exampleStorage'
  putStrLn ("" :: Text)
  putStrLn ("exampleStorage:" :: Text)
  print $ printLorentzValue False exampleStorage


data Entrypoints
  = Admin AdminEntrypoints
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

