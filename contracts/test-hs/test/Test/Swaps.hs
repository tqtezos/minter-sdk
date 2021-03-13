module Test.Swaps
  ( test_SomeTest
  ) where

import Test.Tasty (TestTree)

import Morley.Nettest.Tasty (nettestScenarioCaps)

import Lorentz.Contracts.Swaps ()

test_SomeTest :: [TestTree]
test_SomeTest =
  [ nettestScenarioCaps "Some check" $ do
      pass
  ]
