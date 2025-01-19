module Effectful.Zoo.Log.Dynamic
  ( Log (..),
    runLog,

    log,
    trace,
    debug,
    info,
    warn,
    error,
    crit,
  ) where

import Effectful.Zoo.Log.Dynamic.Api
import Effectful.Zoo.Log.Dynamic.Effect