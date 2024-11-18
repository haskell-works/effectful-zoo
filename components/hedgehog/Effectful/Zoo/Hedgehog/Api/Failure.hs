module Effectful.Zoo.Hedgehog.Api.Failure
  ( failException,
    failWith,
    failWithCallStack,

    catchAssertion,
    throwAssertion,
  ) where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Zoo.Core
import Effectful.Zoo.Hedgehog.Effect
import HaskellWorks.Prelude
import Hedgehog.Internal.Property qualified as H
import Hedgehog.Internal.Source qualified as H

failException :: forall a es. ()
  => HasCallStack
  => es <: Hedgehog
  => SomeException
  -> Eff es a
failException e =
  withFrozenCallStack $
    H.failException e

failWith :: forall a es. ()
  => HasCallStack
  => es <: Hedgehog
  => Maybe H.Diff
  -> String
  -> Eff es a
failWith diff msg =
  withFrozenCallStack $
    H.failWith diff msg

failWithCallStack :: forall a es. ()
  => es <: Hedgehog
  => CallStack
  -> Maybe H.Diff
  -> String
  -> Eff es a
failWithCallStack cs diff msg =
  withFrozenCallStack $
    throwAssertion (H.Failure (H.getCaller cs) msg diff)

catchAssertion :: forall a es. ()
  => HasCallStack
  => es <: Hedgehog
  => Eff es a 
  -> (H.Failure -> Eff es a) 
  -> Eff es a
catchAssertion m h =
  withFrozenCallStack $
    send $ CatchAssertion m h

throwAssertion :: forall a es. ()
  => HasCallStack
  => es <: Hedgehog
  => H.Failure
  -> Eff es a
throwAssertion e =
  withFrozenCallStack $
    send $ ThrowAssertion e
