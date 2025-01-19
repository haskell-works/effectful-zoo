module Effectful.Zoo.DataLog.Static
  ( DataLog,
    runDataLog,
    runDataLogTextToHandle,
    runDataLogTextToStdout,
    runDataLogTextToStderr,
    getDataLogger,
    withDataLog,
    log,
    local,
  ) where

import Effectful.Zoo.DataLog.Static.Api
import Effectful.Zoo.DataLog.Static.Effect
