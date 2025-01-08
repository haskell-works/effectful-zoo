module Effectful.Zoo.Log.Api.LazyText
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
import Effectful.Zoo.Log.Dynamic
import HaskellWorks.Prelude

log :: ()
  => HasCallStack
  => r <: Log LazyText
  => Severity
  -> LazyText
  -> Eff r ()
log severity message =
  withFrozenCallStack $
    send (Log (LogMessage severity message))

trace :: ()
  => HasCallStack
  => r <: Log LazyText
  => LazyText
  -> Eff r ()
trace =
  withFrozenCallStack $
    log Trace

debug :: ()
  => HasCallStack
  => r <: Log LazyText
  => LazyText
  -> Eff r ()
debug =
  withFrozenCallStack $
    log Debug

info :: ()
  => HasCallStack
  => r <: Log LazyText
  => LazyText
  -> Eff r ()
info =
  withFrozenCallStack $
    log Info

warn :: ()
  => HasCallStack
  => r <: Log LazyText
  => LazyText
  -> Eff r ()
warn =
  withFrozenCallStack $
    log Warn

error :: ()
  => HasCallStack
  => r <: Log LazyText
  => LazyText
  -> Eff r ()
error =
  withFrozenCallStack $
    log Error

crit :: ()
  => HasCallStack
  => r <: Log LazyText
  => LazyText
  -> Eff r ()
crit =
  withFrozenCallStack $
    log Crit
