module Effectful.Zoo.Hedgehog.Api.Tasty
  ( UnitTest,
    unitTest,
  ) where

import HaskellWorks.Prelude
import Hedgehog (TestT)
import Hedgehog qualified as H
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Hedgehog (testProperty)

type UnitTest = TestT IO ()

unitTest :: ()
  => TestName
  -> TestT IO ()
  -> TestTree
unitTest desc =
  testProperty desc . H.withTests 1 . H.property . H.test
