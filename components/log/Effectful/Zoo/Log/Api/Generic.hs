module Effectful.Zoo.Log.Api.Generic
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
import Effectful.Zoo.Log.Data.Severity
import Effectful.Zoo.Log.Dynamic
import HaskellWorks.Prelude hiding (log)

log :: ()
  => HasCallStack
  => r <: Log i
  => Severity
  -> i
  -> Eff r ()
log severity message =
  withFrozenCallStack $
    send (Log severity message)

trace :: ()
  => HasCallStack
  => r <: Log i
  => i
  -> Eff r ()
trace =
  withFrozenCallStack $
    log Trace

debug :: ()
  => HasCallStack
  => r <: Log i
  => i
  -> Eff r ()
debug =
  withFrozenCallStack $
    log Debug

info :: ()
  => HasCallStack
  => r <: Log i
  => i
  -> Eff r ()
info =
  withFrozenCallStack $
    log Info

warn :: ()
  => HasCallStack
  => r <: Log i
  => i
  -> Eff r ()
warn =
  withFrozenCallStack $
    log Warn

error :: ()
  => HasCallStack
  => r <: Log i
  => i
  -> Eff r ()
error =
  withFrozenCallStack $
    log Error

crit :: ()
  => HasCallStack
  => r <: Log i
  => i
  -> Eff r ()
crit =
  withFrozenCallStack $
    log Crit
