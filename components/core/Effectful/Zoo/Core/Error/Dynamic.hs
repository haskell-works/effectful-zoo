module Effectful.Zoo.Core.Error.Dynamic
  ( Error,
    catch,
    catch_,
    throw,
    trap,
    trap_,

    catchWithCallStack,
    catchWithCallStack_,
    trapWithCallStack,
    trapWithCallStack_,
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

catchWithCallStack_ :: forall e es a. ()
  => HasCallStack
  => es <: Error e
  => Eff es a
  -> (CallStack -> Eff es a)
  -> Eff es a
catchWithCallStack_ f h =
  catchWithCallStack @e f \cs _ -> h cs

trapWithCallStack :: forall e es a. ()
  => HasCallStack
  => es <: Error e
  => (CallStack -> e -> Eff es a)
  -> Eff es a
  -> Eff es a
trapWithCallStack =
  flip catchWithCallStack

trapWithCallStack_ :: forall e es a. ()
  => HasCallStack
  => es <: Error e
  => (CallStack -> Eff es a)
  -> Eff es a
  -> Eff es a
trapWithCallStack_ h =
  trapWithCallStack @e (const . h)

catch :: forall e es a. ()
  => HasCallStack
  => es <: Error e
  => Eff es a
  -> (e -> Eff es a)
  -> Eff es a
catch action handler =
  catchWithCallStack action (const handler)

catch_ :: forall e es a. ()
  => HasCallStack
  => es <: Error e
  => Eff es a
  -> Eff es a
  -> Eff es a
catch_ action handler =
  catch @e action (const handler)

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

trap_ :: forall e es a. ()
  => HasCallStack
  => es <: Error e
  => Eff es a
  -> Eff es a
  -> Eff es a
trap_ handler =
  trap @e (const handler)
