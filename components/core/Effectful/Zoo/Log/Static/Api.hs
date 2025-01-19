module Effectful.Zoo.Log.Static.Api
  ( log,
    trace,
    debug,
    info,
    warn,
    error,
    crit,
  ) where

import Effectful
import Effectful.Dispatch.Static
import Effectful.Zoo.Core
import Effectful.Zoo.Log.Data.LogMessage
import Effectful.Zoo.Log.Data.Severity
import Effectful.Zoo.Log.Static.Effect
import HaskellWorks.Prelude

log :: forall i r. ()
  => HasCallStack
  => r <: Log i
  => Severity
  -> i
  -> Eff r ()
log severity message =
  withFrozenCallStack $
    logMessage $ LogMessage severity message

trace :: forall i r. ()
  => HasCallStack
  => r <: Log i
  => i
  -> Eff r ()
trace =
  withFrozenCallStack $
    log Trace

debug :: forall i r. ()
  => HasCallStack
  => r <: Log i
  => i
  -> Eff r ()
debug =
  withFrozenCallStack $
    log Debug

info :: forall i r. ()
  => HasCallStack
  => r <: Log i
  => i
  -> Eff r ()
info =
  withFrozenCallStack $
    log Info

warn :: forall i r. ()
  => HasCallStack
  => r <: Log i
  => i
  -> Eff r ()
warn =
  withFrozenCallStack $
    log Warn

error :: forall i r. ()
  => HasCallStack
  => r <: Log i
  => i
  -> Eff r ()
error =
  withFrozenCallStack $
    log Error

crit :: forall i r. ()
  => HasCallStack
  => r <: Log i
  => i
  -> Eff r ()
crit =
  withFrozenCallStack $
    log Crit
