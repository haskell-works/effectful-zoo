module Effectful.Zoo.DataLog.Dynamic.Api
  ( dataLog,
  ) where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Zoo.Core
import Effectful.Zoo.DataLog.Dynamic.Effect
import HaskellWorks.Prelude

dataLog :: forall i r. ()
  => HasCallStack
  => r <: DataLog i
  => i
  -> Eff r ()
dataLog i =
  withFrozenCallStack do
    send $ DataLog i
