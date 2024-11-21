module Effectful.Zoo.DataLog.Api
  ( dataLog,
  ) where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Zoo.Core
import Effectful.Zoo.DataLog.Dynamic
import HaskellWorks.Prelude

dataLog :: ()
  => HasCallStack
  => r <: DataLog i
  => i
  -> Eff r ()
dataLog i =
  withFrozenCallStack do
    send $ DataLog i
{-# inline dataLog #-}
