module Effectful.Zoo.Hedgehog.Api.Assert
  ( assert,
    (===),
    (/==),
  ) where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Zoo.Core
import Effectful.Zoo.Hedgehog.Effect
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
