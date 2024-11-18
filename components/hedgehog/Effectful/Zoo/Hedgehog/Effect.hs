{-# OPTIONS_GHC -fno-warn-orphans #-}

module Effectful.Zoo.Hedgehog.Effect
  ( Hedgehog(..),
    MonadTest(..),
  ) where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Zoo.Core
import HaskellWorks.Prelude
import Hedgehog (MonadTest(..))
import Hedgehog qualified as H
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
