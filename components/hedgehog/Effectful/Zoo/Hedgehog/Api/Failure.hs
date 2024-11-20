module Effectful.Zoo.Hedgehog.Api.Failure
  ( failure,
    failException,
    failWith,
    failWithCallStack,

    catchAssertion,
    throwAssertion,
  ) where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Zoo.Core
import Effectful.Zoo.Hedgehog.Dynamic
import HaskellWorks.Prelude
import Hedgehog.Internal.Property qualified as H
import Hedgehog.Internal.Source qualified as H

failure :: forall a r. ()
  => HasCallStack
  => r <: Hedgehog
  => Eff r a
failure =
  withFrozenCallStack
    H.failure

failException :: forall a r. ()
  => HasCallStack
  => r <: Hedgehog
  => SomeException
  -> Eff r a
failException e =
  withFrozenCallStack $
    H.failException e

failWith :: forall a r. ()
  => HasCallStack
  => r <: Hedgehog
  => Maybe H.Diff
  -> String
  -> Eff r a
failWith diff msg =
  withFrozenCallStack $
    H.failWith diff msg

failWithCallStack :: forall a r. ()
  => r <: Hedgehog
  => CallStack
  -> Maybe H.Diff
  -> String
  -> Eff r a
failWithCallStack cs diff msg =
  withFrozenCallStack $
    throwAssertion (H.Failure (H.getCaller cs) msg diff)

catchAssertion :: forall a r. ()
  => HasCallStack
  => r <: Hedgehog
  => Eff r a 
  -> (H.Failure -> Eff r a) 
  -> Eff r a
catchAssertion m h =
  withFrozenCallStack $
    send $ CatchAssertion m h

throwAssertion :: forall a r. ()
  => HasCallStack
  => r <: Hedgehog
  => H.Failure
  -> Eff r a
throwAssertion e =
  withFrozenCallStack $
    send $ ThrowAssertion e
