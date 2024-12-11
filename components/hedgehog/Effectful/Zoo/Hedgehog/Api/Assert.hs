module Effectful.Zoo.Hedgehog.Api.Assert
  ( assert,
    (===),
    (/==),
    onNothingFail,
    onNothingFailM,
    onLeftFail,
    onLeftFailM,
  ) where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Zoo.Core
import Effectful.Zoo.Hedgehog.Api.Failure
import Effectful.Zoo.Hedgehog.Dynamic
import HaskellWorks.Prelude
import Hedgehog qualified as H

infix 4 ===, /==

assert :: forall r. ()
  => HasCallStack
  => r <: Hedgehog
  => Bool
  -> Eff r ()
assert condition =
  withFrozenCallStack $
    H.assert condition

(===) :: forall a r. ()
  => HasCallStack
  => Eq a
  => Show a
  => r <: Hedgehog
  => a
  -> a
  -> Eff r ()
(===) a b =
  withFrozenCallStack $
    a H.=== b

(/==) :: forall a r. ()
  => HasCallStack
  => Eq a
  => Show a
  => r <: Hedgehog
  => a
  -> a
  -> Eff r ()
(/==) a b =
  withFrozenCallStack $
    a H./== b

onNothingFail :: forall a r. ()
  => HasCallStack
  => r <: Hedgehog
  => Maybe a
  -> Eff r a
onNothingFail mv =
  withFrozenCallStack $
    case mv of
      Just a -> pure a
      Nothing -> failWith Nothing "Expected Just, but got Nothing"

onNothingFailM :: forall a r. ()
  => HasCallStack
  => r <: Hedgehog
  => Eff r (Maybe a)
  -> Eff r a
onNothingFailM f =
  withFrozenCallStack $
    f >>= onNothingFail

onLeftFail :: forall e a r. ()
  => HasCallStack
  => Show e
  => r <: Hedgehog
  => Either e a
  -> Eff r a
onLeftFail ea =
  withFrozenCallStack $
    case ea of
      Right a -> pure a
      Left e -> failWith Nothing $ "Expected Just, but got Left: " <> show e

onLeftFailM :: forall e a r. ()
  => HasCallStack
  => Show e
  => r <: Hedgehog
  => Eff r (Either e a)
  -> Eff r a
onLeftFailM f =
  withFrozenCallStack $
    f >>= onLeftFail
