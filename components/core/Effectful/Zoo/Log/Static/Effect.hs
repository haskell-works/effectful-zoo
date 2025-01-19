module Effectful.Zoo.Log.Static.Effect
  ( Log,
    runLog,
    runLogToHandle,
    runLogToStdout,
    runLogToStderr,
    withLog,
    logMessage,
    localLog,
  ) where

import Data.Kind
import Data.Text.IO qualified as T
import Effectful
import Effectful.Dispatch.Static
import Effectful.Zoo.Core
import Effectful.Zoo.Log.Data.Logger
import Effectful.Zoo.Log.Data.LogMessage
import GHC.Stack qualified as GHC
import HaskellWorks.Prelude
import System.IO qualified as IO

data Log (i :: Type) :: Effect

type instance DispatchOf (Log i) = Static NoSideEffects

newtype instance StaticRep (Log i) = Log (Logger i)

runLog :: forall i a r. ()
  => r <: IOE
  => HasCallStack
  => UnliftStrategy
  -> (CallStack -> LogMessage i -> Eff r ())
  -> Eff (Log i : r) a
  -> Eff r a
runLog strategy run f = do
  s <- mkLogger strategy run
  evalStaticRep (Log s) f

runLogToHandle :: ()
  => HasCallStack
  => Handle
  -> (LogMessage a -> Text)
  -> Eff (Log a : r) a
  -> Eff r a
runLogToHandle h f =
  evalStaticRep $ Log $ Logger $ \_ m ->
    T.hPutStrLn h $ f m

runLogToStdout :: ()
  => HasCallStack
  => (LogMessage a -> Text)
  -> Eff (Log a : r) a
  -> Eff r a
runLogToStdout =
  runLogToHandle IO.stdout

runLogToStderr :: ()
  => HasCallStack
  => (LogMessage a -> Text)
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
  logger <- getLogger
  let _ = logger
  raise $ evalStaticRep (Log (f logger)) m

withLog :: ()
  => HasCallStack
  => (o -> i)
  -> Eff (Log o : r) a
  -> Eff (Log i : r) a
withLog =
  withDataLogSerialiser . contramap

getLogger :: ()
  => HasCallStack
  => r <: Log i
  => Eff r (Logger i)
getLogger = do
  Log i <- getStaticRep
  pure i

logMessage :: ()
  => HasCallStack
  => r <: Log i
  => LogMessage i
  -> Eff r ()
logMessage m =
  withFrozenCallStack do
    dataLogger <- getLogger
    unsafeEff_ $ dataLogger.run GHC.callStack m

localLog :: ()
  => HasCallStack
  => r <: Log i
  => (i -> i)
  -> Eff r a
  -> Eff r a
localLog f =
  localStaticRep $ \(Log s) -> Log (contramap f s)
