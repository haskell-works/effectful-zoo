module Effectful.Zoo.Log.Static
  ( Log,
    runLog,
    runLogToHandle,
    runLogToStdout,
    runLogToStderr,
    withLog,
    logMessage,
    localLog,

    log,
    trace,
    debug,
    info,
    warn,
    error,
    crit,
  ) where

import Effectful.Zoo.Log.Static.Api

import Effectful.Zoo.Log.Static.Effect
