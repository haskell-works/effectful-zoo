module Effectful.Zoo.Core.Error.Dynamic
  ( Error,
    catch,
    throw,
    trap,

    catchWithCallStack,
    trapWithCallStack,
  ) where

import Effectful
import Effectful.Error.Dynamic (Error)
import Effectful.Error.Dynamic qualified as E
import Effectful.Zoo.Core
import HaskellWorks.Prelude

catchWithCallStack :: forall e es a. ()
  => HasCallStack
  => es <: Error e
  => Eff es a
  -> (CallStack -> e -> Eff es a)
  -> Eff es a
catchWithCallStack =
  E.catchError

trapWithCallStack :: forall e es a. ()
  => HasCallStack
  => es <: Error e
  => (CallStack -> e -> Eff es a)
  -> Eff es a
  -> Eff es a
trapWithCallStack =
  flip catchWithCallStack

catch :: forall e es a. ()
  => HasCallStack
  => es <: Error e
  => Eff es a
  -> (e -> Eff es a)
  -> Eff es a
catch action handler =
  catchWithCallStack action (const handler)

throw :: forall e es a. ()
  => HasCallStack
  => es <: Error e
  => Show e
  => e
  -> Eff es a
throw =
  E.throwError

trap :: forall e es a. ()
  => HasCallStack
  => es <: Error e
  => (e -> Eff es a)
  -> Eff es a
  -> Eff es a
trap =
  flip catch
