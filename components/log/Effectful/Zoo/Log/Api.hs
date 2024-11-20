module Effectful.Zoo.Log.Api
  ( log,
    trace,
    debug,
    info,
    warn,
    error,
    crit,
  ) where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Zoo.Core
import Effectful.Zoo.Log.Data.LogMessage
import Effectful.Zoo.Log.Data.Severity
import Effectful.Zoo.Log.Effect
import HaskellWorks.Prelude hiding (log)

log :: ()
  => HasCallStack
  => r <: Log
  => Severity
  -> Text
  -> Eff r ()
log severity message =
  withFrozenCallStack $
    send (Log (LogMessage severity message))

trace :: ()
  => HasCallStack
  => r <: Log
  => Text
  -> Eff r ()
trace =
  withFrozenCallStack $
    log Trace

debug :: ()
  => HasCallStack
  => r <: Log
  => Text
  -> Eff r ()
debug =
  withFrozenCallStack $
    log Debug

info :: ()
  => HasCallStack
  => r <: Log
  => Text
  -> Eff r ()
info =
  withFrozenCallStack $
    log Info

warn :: ()
  => HasCallStack
  => r <: Log
  => Text
  -> Eff r ()
warn =
  withFrozenCallStack $
    log Warn

error :: ()
  => HasCallStack
  => r <: Log
  => Text
  -> Eff r ()
error =
  withFrozenCallStack $
    log Error

crit :: ()
  => HasCallStack
  => r <: Log
  => Text
  -> Eff r ()
crit =
  withFrozenCallStack $
    log Crit
