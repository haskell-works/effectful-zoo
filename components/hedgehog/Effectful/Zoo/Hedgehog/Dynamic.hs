{-# OPTIONS_GHC -fno-warn-orphans #-}

module Effectful.Zoo.Hedgehog.Dynamic
  ( Hedgehog(..),
    MonadTest(..),

    runHedgehogIO,
    runHedgehog,
  ) where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Writer.Static.Local
import Effectful.Zoo.Core
import Effectful.Zoo.Hedgehog.MonadTestProxy
import HaskellWorks.Prelude
import Hedgehog (MonadTest(..))
import Hedgehog qualified as H
import Hedgehog.Internal.Property (Failure, Journal)
import Hedgehog.Internal.Property qualified as H

data Hedgehog :: Effect where
  CatchAssertion :: HasCallStack
    => m a
    -> (H.Failure -> m a)
    -> Hedgehog m a

  LiftTest :: HasCallStack
    => H.Test a
    -> Hedgehog m a

  ThrowAssertion :: HasCallStack
    => H.Failure
    -> Hedgehog m a

type instance DispatchOf Hedgehog = Dynamic

instance (r <: Hedgehog) => MonadTest (Eff r) where
  liftTest t = send $ LiftTest t

runHedgehogIO :: forall a. ()
  => Eff
      [ Hedgehog
      , Error Failure
      , Writer Journal
      , IOE
      ] a
  -> IO (Either Failure a, Journal)
runHedgehogIO f =
  f
    & runHedgehog
    & runError @H.Failure
    & runWriter @H.Journal
    & fmap (first (first snd))
    & runEff

runHedgehog :: forall a r. ()
  => r <: Error Failure
  => r <: Writer Journal
  => Eff (Hedgehog : r) a
  -> Eff r a
runHedgehog =
  interpret $ \env -> \case
    CatchAssertion f h -> localUnlift env SeqUnlift $ \unlift -> catchError (unlift f) (const (unlift . h))
    LiftTest f -> liftTestProxy f
    ThrowAssertion failure -> throwError failure
