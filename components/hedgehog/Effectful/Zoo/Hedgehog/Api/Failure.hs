module Effectful.Zoo.Hedgehog.Api.Failure
  ( H.Failure,
    failWithCallStack,
  ) where

import Effectful
import Effectful.Zoo.Core
import Effectful.Zoo.Error.Static
import HaskellWorks.Prelude
import Hedgehog.Internal.Property qualified as H
import Hedgehog.Internal.Source qualified as H

failWithCallStack :: forall a r. ()
  => r <: Error H.Failure
  => CallStack
  -> Maybe H.Diff
  -> String
  -> Eff r a
failWithCallStack cs diff msg =
  withFrozenCallStack $
    throw (H.Failure (H.getCaller cs) msg diff)
