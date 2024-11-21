module Effectful.Zoo.Log.Static
  ( Log,
    runLog,
    runLogToHandle,
    runLogToStdout,
    runLogToStderr,
    withLog,
    log,
    local,
  ) where

import Data.Kind
import Data.Text.IO qualified as T
import Effectful
import Effectful.Dispatch.Static
import Effectful.Zoo.Core
import Effectful.Zoo.Log.Data.Logger
import Effectful.Zoo.Log.Data.Severity
import GHC.Stack qualified as GHC
import HaskellWorks.Prelude
import System.IO qualified as IO

data Log (i :: Type) :: Effect

type instance DispatchOf (Log i) = Static NoSideEffects

newtype instance StaticRep (Log i) = Log (Logger i)

runLog :: ()
  => r <: IOE
  => HasCallStack
  => UnliftStrategy
  -> (CallStack -> Severity -> i -> Eff r ())
  -> Eff (Log i : r) a
  -> Eff r a
runLog strategy run f = do
  s <- mkLogger strategy run
  evalStaticRep (Log s) f

runLogToHandle :: ()
  => HasCallStack
  => Handle
  -> (Severity -> a -> Text)
  -> Eff (Log a : r) a
  -> Eff r a
runLogToHandle h f =
  evalStaticRep $ Log $ Logger $ \_ severity i ->
    T.hPutStrLn h $ f severity i

runLogToStdout :: ()
  => HasCallStack
  => (Severity -> a -> Text)
  -> Eff (Log a : r) a
  -> Eff r a
runLogToStdout =
  runLogToHandle IO.stdout

runLogToStderr :: ()
  => HasCallStack
  => (Severity -> a -> Text)
  -> Eff (Log a : r) a
  -> Eff r a
runLogToStderr =
  runLogToHandle IO.stderr

withDataLogSerialiser :: ()
  => HasCallStack
  => (Logger i -> Logger o)
  -> Eff (Log o : r) a
  -> Eff (Log i : r) a
withDataLogSerialiser f m = do
  logger <- getDataLogger
  let _ = logger
  raise $ evalStaticRep (Log (f logger)) m

withLog :: ()
  => HasCallStack
  => (o -> i)
  -> Eff (Log o : r) a
  -> Eff (Log i : r) a
withLog =
  withDataLogSerialiser . contramap

getDataLogger :: ()
  => HasCallStack
  => r <: Log i
  => Eff r (Logger i)
getDataLogger = do
  Log i <- getStaticRep
  pure i

log :: ()
  => HasCallStack
  => r <: Log i
  => r <: IOE
  => Severity
  -> i
  -> Eff r ()
log severity i =
  withFrozenCallStack do
    dataLogger <- getDataLogger
    liftIO $ dataLogger.run GHC.callStack severity i

local :: ()
  => HasCallStack
  => r <: Log i
  => (i -> i)
  -> Eff r a
  -> Eff r a
local f =
  localStaticRep $ \(Log s) -> Log (contramap f s)
