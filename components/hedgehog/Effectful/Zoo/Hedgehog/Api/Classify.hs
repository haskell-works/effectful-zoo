module Effectful.Zoo.Hedgehog.Api.Classify
  ( classify,
  ) where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Zoo.Core
import Effectful.Zoo.Hedgehog.Dynamic
import HaskellWorks.Prelude
import Hedgehog qualified as H

classify :: forall r. ()
  => HasCallStack
  => r <: Hedgehog
  => H.LabelName
  -> Bool
  -> Eff r ()
classify name b =
  withFrozenCallStack $
    H.classify name b
