module Main where

import Effectful.Zoo.Hedgehog.Api.Tasty
import Effectful.Zoo.Hedgehog.Test.HedgehogTest (test_simple)
import Effectful.Zoo.Hedgehog.Test.PropertySpec
import Test.Tasty (TestTree, defaultMain, testGroup)
import HaskellWorks.Prelude

tests :: TestTree
tests =
  testGroup "all"
    [ toTestTree "Simple test" test_simple
    , toTestTree "Simple property spec" property_spec
    , toTestTree "Simple test spec" test_spec
    ]

main :: IO ()
main =
  defaultMain tests
