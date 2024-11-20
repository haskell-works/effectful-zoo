module Main where

import Effectful.Zoo.Hedgehog (unitTest)
import Effectful.Zoo.Hedgehog.Test.HedgehogTest (test_simple)
import Test.Tasty (TestTree, defaultMain, testGroup)
import HaskellWorks.Prelude

tests :: TestTree
tests =
  testGroup "all" [
    unitTest "Simple test" test_simple
  ]

main :: IO ()
main =
  defaultMain tests
