{- HLINT ignore "Eta reduce" -}

{-# OPTIONS_GHC -Wno-orphans #-}

module Effectful.Zoo.Hedgehog.Effect.Hedgehog
  ( Hedgehog,
    HedgehogEnv(..),
    runHedgehogProperty,
    runHedgehogUnit,
  ) where

import Control.Concurrent.STM qualified as IO
import Control.Monad
import Control.Monad.Catch (MonadThrow(..))
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.STM
import Effectful.Dispatch.Static
import Effectful.Zoo.Core
import Effectful.Zoo.Error.Static
import Effectful.Zoo.Hedgehog.Api.MonadAssertion
import Effectful.Zoo.Hedgehog.Data.TestResult
import HaskellWorks.Prelude
import Hedgehog (MonadTest(..))
import Hedgehog qualified as H
import Hedgehog.Internal.Property qualified as H

-- | An effect for interacting with the filesystem.
data Hedgehog :: Effect

type instance DispatchOf Hedgehog = Static WithSideEffects
newtype instance StaticRep Hedgehog = Hedgehog HedgehogEnv

data HedgehogEnv
  = PropertyEnv (TMVar (H.PropertyT IO ()))
  | UnitTestEnv (TMVar (H.TestT IO ()))

instance {-# OVERLAPS #-}
    ( r <: Concurrent
    , r <: Error H.Failure
    , r <: Hedgehog
    ) => MonadTest (Eff r) where
  liftTest f = do
    Hedgehog env <- getStaticRep
    mvA <- newEmptyTMVarIO
    case env of
      PropertyEnv mvAction ->
        atomically $ putTMVar mvAction (tryExceptAssertion (liftTest f) >>= liftIO . IO.atomically . IO.putTMVar mvA)
      UnitTestEnv mvAction ->
        atomically $ putTMVar mvAction (tryExceptAssertion (liftTest f) >>= liftIO . IO.atomically . IO.putTMVar mvA)
    testResult <- atomically $ takeTMVar mvA
    getTestResult testResult

instance
    ( r <: Error H.Failure
    ) => MonadAssertion (Eff r) where
  throwAssertion f = throw f
  catchAssertion g h = g & trapIn h

getTestResult :: ()
  => r <: Error H.Failure
  => TestResult a
  -> Eff r a
getTestResult = \case
  TestResult a -> pure a
  TestFailure f -> throw f
  TestError e -> throwM e

runHedgehogProperty :: ()
  => r <: IOE
  => TMVar (H.PropertyT IO ())
  -> Eff (Hedgehog : r) a
  -> Eff r a
runHedgehogProperty tvAction =
  evalStaticRep (Hedgehog (PropertyEnv tvAction))

runHedgehogUnit :: ()
  => r <: IOE
  => TMVar (H.TestT IO ())
  -> Eff (Hedgehog : r) a
  -> Eff r a
runHedgehogUnit tvAction =
  evalStaticRep (Hedgehog (UnitTestEnv tvAction))
