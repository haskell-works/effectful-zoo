module Effectful.Zoo.Hedgehog.Api.Tasty
  ( PropertyTest,
    UnitTest,

    property,
    unitTest,
  ) where

import HaskellWorks.Prelude
import Hedgehog (PropertyT, TestT)
import Hedgehog qualified as H
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Hedgehog (testProperty)

type PropertyTest = PropertyT IO ()

type UnitTest = TestT IO ()

unitTest :: ()
  => TestName
  -> TestT IO ()
  -> TestTree
unitTest desc =
  testProperty desc . H.withTests 1 . H.property . H.test

property :: ()
  => TestName
  -> PropertyT IO ()
  -> TestTree
property desc =
  testProperty desc . H.property
