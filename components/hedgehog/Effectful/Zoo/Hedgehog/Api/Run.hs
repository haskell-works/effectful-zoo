module Effectful.Zoo.Hedgehog.Api.Run
  ( hedgehog,
  ) where

import Control.Monad.Trans.Writer.Lazy qualified as MTL
import Effectful
import Effectful.Environment
import Effectful.FileSystem
import Effectful.Error.Static
import Effectful.Writer.Static.Local
import Effectful.Zoo.Hedgehog.Api.Journal
import Effectful.Zoo.Hedgehog.Dynamic
import Effectful.Zoo.Log.Dynamic
import HaskellWorks.Prelude
import Hedgehog qualified as H
import Hedgehog.Internal.Property (Failure, Journal)
import Hedgehog.Internal.Property qualified as H

hedgehog :: forall a. ()
  => Eff
      [ Log Text
      , Environment
      , FileSystem
      , Hedgehog
      , Error Failure
      , Writer Journal
      , IOE
      ] a
  -> H.TestT IO a
hedgehog f =
  f
    & runLog (ConcUnlift Persistent Unlimited) jotLogTextWithCallStack
    & runEnvironment
    & runFileSystem
    & runHedgehogIO
    & MTL.WriterT
    & ExceptT
    & H.TestT
