module Effectful.Zoo.DataLog.Static
  ( DataLog,
    runDataLog,
    runDataLogTextToHandle,
    runDataLogTextToStdout,
    runDataLogTextToStderr,
    getDataLogger,
    withDataLog,
    dataLog,
    localDataLog,
  ) where

import Effectful.Zoo.DataLog.Static.Api
import Effectful.Zoo.DataLog.Static.Effect
