module Effectful.Zoo.DataLog.Static.Api
  ( log,
  ) where

import Effectful
import Effectful.Dispatch.Static
import Effectful.Zoo.Core
import Effectful.Zoo.DataLog.Data.DataLogger
import Effectful.Zoo.DataLog.Static.Effect
import HaskellWorks.Prelude

log :: ()
  => HasCallStack
  => r <: DataLog i
  => i
  -> Eff r ()
log i = do
  dataLogger <- getDataLogger
  unsafeEff_ $ dataLogger.run i
