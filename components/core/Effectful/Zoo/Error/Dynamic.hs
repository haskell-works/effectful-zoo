module Effectful.Zoo.Error.Dynamic
  ( Error,

    throw,

    catch,
    catch_,
    trap,
    trap_,

    catchWithCallStack,
    catchWithCallStack_,
    trapWithCallStack,
    trapWithCallStack_,

    catchIn,
    catchIn_,
    trapIn,
    trapIn_,

    catchWithCallStackIn,
    catchWithCallStackIn_,
    trapWithCallStackIn,
    trapWithCallStackIn_,

    fromEither,
    mapError,
    runError,
    runError_,
    runErrorMap_,
  ) where

import Effectful
import Effectful.Error.Dynamic (Error, runError)
import Effectful.Error.Dynamic qualified as E
import Effectful.Zoo.Core
import HaskellWorks.Prelude

throw :: forall e r a. ()
  => HasCallStack
  => r <: Error e
  => Show e
  => e
  -> Eff r a
throw =
  E.throwError

catchWithCallStack :: forall e r a. ()
  => HasCallStack
  => Eff (Error e : r) a
  -> (CallStack -> e -> Eff r a)
  -> Eff r a
catchWithCallStack action handler =
  E.runError action
    & onLeftM (uncurry handler)

catchWithCallStack_ :: forall e r a. ()
  => HasCallStack
  => Eff (Error e : r) a
  -> (CallStack -> Eff r a)
  -> Eff r a
catchWithCallStack_ f h =
  catchWithCallStack @e f \cs _ -> h cs

trapWithCallStack :: forall e r a. ()
  => HasCallStack
  => (CallStack -> e -> Eff r a)
  -> Eff (Error e : r) a
  -> Eff r a
trapWithCallStack =
  flip catchWithCallStack

trapWithCallStack_ :: forall e r a. ()
  => HasCallStack
  => (CallStack -> Eff r a)
  -> Eff (Error e : r) a
  -> Eff r a
trapWithCallStack_ h =
  trapWithCallStack @e (const . h)

catch :: forall e r a. ()
  => Eff (Error e : r) a
  -> (e -> Eff r a)
  -> Eff r a
catch action handler =
  E.runError action
    & onLeftM (handler . snd)

catch_ :: forall e r a. ()
  => Eff (Error e : r) a
  -> Eff r a
  -> Eff r a
catch_ action handler =
  catch @e action (const handler)

trap :: forall e r a. ()
  => (e -> Eff r a)
  -> Eff (Error e : r) a
  -> Eff r a
trap =
  flip catch

trap_ :: forall e r a. ()
  => Eff r a
  -> Eff (Error e : r) a
  -> Eff r a
trap_ handler =
  trap @e (const handler)

catchWithCallStackIn :: forall e es a. ()
  => HasCallStack
  => es <: Error e
  => Eff es a
  -> (CallStack -> e -> Eff es a)
  -> Eff es a
catchWithCallStackIn =
  E.catchError

catchWithCallStackIn_ :: forall e es a. ()
  => HasCallStack
  => es <: Error e
  => Eff es a
  -> (CallStack -> Eff es a)
  -> Eff es a
catchWithCallStackIn_ f h =
  catchWithCallStackIn @e f \cs _ -> h cs

trapWithCallStackIn :: forall e es a. ()
  => HasCallStack
  => es <: Error e
  => (CallStack -> e -> Eff es a)
  -> Eff es a
  -> Eff es a
trapWithCallStackIn =
  flip catchWithCallStackIn

trapWithCallStackIn_ :: forall e es a. ()
  => HasCallStack
  => es <: Error e
  => (CallStack -> Eff es a)
  -> Eff es a
  -> Eff es a
trapWithCallStackIn_ h =
  trapWithCallStackIn @e (const . h)

catchIn :: forall e es a. ()
  => HasCallStack
  => es <: Error e
  => Eff es a
  -> (e -> Eff es a)
  -> Eff es a
catchIn action handler =
  catchWithCallStackIn action (const handler)

catchIn_ :: forall e es a. ()
  => HasCallStack
  => es <: Error e
  => Eff es a
  -> Eff es a
  -> Eff es a
catchIn_ action handler =
  catchIn @e action (const handler)

trapIn :: forall e es a. ()
  => HasCallStack
  => es <: Error e
  => (e -> Eff es a)
  -> Eff es a
  -> Eff es a
trapIn =
  flip catchIn

trapIn_ :: forall e es a. ()
  => HasCallStack
  => es <: Error e
  => Eff es a
  -> Eff es a
  -> Eff es a
trapIn_ handler =
  trapIn @e (const handler)

fromEither :: forall e a r. ()
  => Show e
  => r <: Error e
  => Either e a
  -> Eff r a
fromEither =
  either throw pure

mapError :: forall d e a r. ()
  => HasCallStack
  => r <: Error e
  => Show e
  => (d -> e)
  -> Eff (Error d : r) a
  -> Eff r a
mapError f g =
  g & trap (throw . f)

runError_ :: ()
  => Eff (Error e : r) a
  -> Eff r (Either e a)
runError_ =
  fmap (first snd) . runError

-- | Run an 'Error' effect and map the error value to a different type.
runErrorMap_ :: ()
  => (e -> d)
  -> Eff (Error e : r) a
  -> Eff r (Either d a)
runErrorMap_ f =
  fmap (first (f . snd)) . runError
