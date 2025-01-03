module Effectful.Zoo.Log.Api.String
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
import HaskellWorks.Prelude

log :: ()
  => HasCallStack
  => r <: Log String
  => Severity
  -> String
  -> Eff r ()
log severity message =
  withFrozenCallStack $
    send (Log severity message)

trace :: ()
  => HasCallStack
  => r <: Log String
  => String
  -> Eff r ()
trace =
  withFrozenCallStack $
    log Trace

debug :: ()
  => HasCallStack
  => r <: Log String
  => String
  -> Eff r ()
debug =
  withFrozenCallStack $
    log Debug

info :: ()
  => HasCallStack
  => r <: Log String
  => String
  -> Eff r ()
info =
  withFrozenCallStack $
    log Info

warn :: ()
  => HasCallStack
  => r <: Log String
  => String
  -> Eff r ()
warn =
  withFrozenCallStack $
    log Warn

error :: ()
  => HasCallStack
  => r <: Log String
  => String
  -> Eff r ()
error =
  withFrozenCallStack $
    log Error

crit :: ()
  => HasCallStack
  => r <: Log String
  => String
  -> Eff r ()
crit =
  withFrozenCallStack $
    log Crit
