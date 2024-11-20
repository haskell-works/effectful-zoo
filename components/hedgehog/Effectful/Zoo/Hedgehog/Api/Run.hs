module Effectful.Zoo.Hedgehog.Api.Run
  ( UnitTest,

    hedgehog,
    unitTest,
  ) where

import Control.Monad.Trans.Writer.Lazy qualified as MTL
import Effectful
import Effectful.Error.Static
import Effectful.Writer.Static.Local
import Effectful.Zoo.Hedgehog.Api.Journal
import Effectful.Zoo.Hedgehog.Dynamic
import Effectful.Zoo.Log.Dynamic
import HaskellWorks.Prelude
import Hedgehog (TestT)
import Hedgehog qualified as H
import Hedgehog.Internal.Property (Failure, Journal)
import Hedgehog.Internal.Property qualified as H
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Hedgehog (testProperty)

type UnitTest = TestT IO ()

hedgehog :: forall a. ()
  => Eff
      [ Log Text
      , Hedgehog
      , Error Failure
      , Writer Journal
      , IOE
      ] a
  -> H.TestT IO a
hedgehog f =
  f
    & runLog (ConcUnlift Persistent Unlimited) jotLogTextWithCallStack
    & runHedgehogIO
    & MTL.WriterT
    & ExceptT
    & H.TestT

unitTest :: ()
  => TestName 
  -> UnitTest 
  -> TestTree
unitTest desc =
  testProperty desc . H.withTests 1 . H.property . H.test
