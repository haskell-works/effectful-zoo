module Effectful.Zoo.Log.Api
  ( log,
    trace,
    debug,
    info,
    warn,
    error,
    crit,
    defaultRenderLogToText,
  ) where

import Effectful.Zoo.Log.Api.Render
import Effectful.Zoo.Log.Dynamic.Api