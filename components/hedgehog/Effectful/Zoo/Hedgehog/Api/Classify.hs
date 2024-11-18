module Effectful.Zoo.Hedgehog.Api.Classify
  ( classify,
  ) where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Zoo.Core
import Effectful.Zoo.Hedgehog.Effect
import HaskellWorks.Prelude
import Hedgehog qualified as H

classify :: forall es. ()
  => HasCallStack
  => es <: Hedgehog
  => H.LabelName
  -> Bool
  -> Eff es ()
classify name b =
  withFrozenCallStack $
    H.classify name b
