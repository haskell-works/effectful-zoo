module Effectful.Zoo.DataLog.Api
  ( dataLog,
    logEntryToJson,
    logMessageToJson,
    putJsonStdout,
  ) where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Zoo.Core
import Effectful.Zoo.DataLog.Dynamic
import HaskellWorks.Prelude
import Effectful.Zoo.DataLog.Api.Json

dataLog :: forall i r. ()
  => HasCallStack
  => r <: DataLog i
  => i
  -> Eff r ()
dataLog i =
  withFrozenCallStack do
    send $ DataLog i
