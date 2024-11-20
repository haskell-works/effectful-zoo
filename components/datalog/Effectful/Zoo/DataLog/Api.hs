module Effectful.Zoo.DataLog.Api where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Zoo.Core
import Effectful.Zoo.DataLog.Data.LogEntry
import Effectful.Zoo.DataLog.Dynamic
import Effectful.Zoo.Log.Data.LogMessage
import Effectful.Zoo.Log.Data.Severity
import HaskellWorks.Prelude hiding (log)

type Logger =
  DataLog (LogEntry LogMessage)

log :: ()
  => HasCallStack
  => r <: Logger
  => r <: IOE
  => Severity
  -> Text
  -> Eff r ()
log severity message =
  withFrozenCallStack do
    send . DataLog =<< annotate (LogMessage severity message)
{-# inline log #-}

trace :: ()
  => HasCallStack
  => r <: Logger
  => r <: IOE
  => Text
  -> Eff r ()
trace =
  withFrozenCallStack (log Trace)
{-# inline trace #-}

debug :: ()
  => HasCallStack
  => r <: Logger
  => r <: IOE
  => Text
  -> Eff r ()
debug =
  withFrozenCallStack (log Debug)
{-# inline debug #-}

info :: ()
  => HasCallStack
  => r <: Logger
  => r <: IOE
  => Text
  -> Eff r ()
info =
  withFrozenCallStack (log Info)
{-# inline info #-}

warn :: ()
  => HasCallStack
  => r <: Logger
  => r <: IOE
  => Text
  -> Eff r ()
warn =
  withFrozenCallStack (log Warn)
{-# inline warn #-}

error :: ()
  => HasCallStack
  => r <: Logger
  => r <: IOE
  => Text
  -> Eff r ()
error =
  withFrozenCallStack (log Error)
{-# inline error #-}

crit :: ()
  => HasCallStack
  => r <: Logger
  => r <: IOE
  => Text
  -> Eff r ()
crit =
  withFrozenCallStack (log Crit)
{-# inline crit #-}
