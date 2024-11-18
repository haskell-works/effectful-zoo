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

assert :: forall es. ()
  => HasCallStack
  => es <: Hedgehog
  => Bool
  -> Eff es ()
assert condition =
  withFrozenCallStack $
    H.assert condition

(===) :: forall a es. ()
  => HasCallStack
  => Eq a
  => Show a
  => es <: Hedgehog
  => a
  -> a
  -> Eff es ()
(===) a b =
  withFrozenCallStack $
    a H.=== b

(/==) :: forall a es. ()
  => HasCallStack
  => Eq a
  => Show a
  => es <: Hedgehog
  => a
  -> a
  -> Eff es ()
(/==) a b =
  withFrozenCallStack $
    a H./== b
