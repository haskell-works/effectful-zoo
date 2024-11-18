module Effectful.Zoo.Hedgehog (
  module Effectful.Zoo.Hedgehog.Effect,

  runHedgehog,
  interpretHedgehog,

  assert,
  (===),
  (/==),
  evalMaybe,
  evalEither,
  jot_,

  UnitTest,
  unitTest,
) where

import Effectful.Zoo.Hedgehog.Api
import Effectful.Zoo.Hedgehog.Run
import Effectful.Zoo.Hedgehog.Effect (Hedgehog)
import HaskellWorks.Prelude
import Hedgehog (TestT)
import Hedgehog qualified as H
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Hedgehog (testProperty)

type UnitTest = TestT IO ()

unitTest :: ()
  => TestName 
  -> UnitTest 
  -> TestTree
unitTest desc =
  testProperty desc . H.withTests 1 . H.property . H.test
