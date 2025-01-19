module Effectful.Zoo.DataLog.Static.Api
  ( dataLog,
  ) where

import Effectful
import Effectful.Dispatch.Static
import Effectful.Zoo.Core
import Effectful.Zoo.DataLog.Data.DataLogger
import Effectful.Zoo.DataLog.Static.Effect
import HaskellWorks.Prelude

dataLog :: forall i r. ()
  => HasCallStack
  => r <: DataLog i
  => i
  -> Eff r ()
dataLog i = do
  dataLogger <- getDataLogger
  unsafeEff_ $ dataLogger.run i
