module Effectful.Zoo.DataLog.Static
  ( DataLog,
    runDataLog,
    runDataLogTextToHandle,
    runDataLogTextToStdout,
    runDataLogTextToStderr,
    withDataLog,
    log,
    local,
  ) where

import Data.Text.IO qualified as T
import Data.Kind
import Effectful
import Effectful.Zoo.Core
import Effectful.Dispatch.Static
import Effectful.Zoo.DataLog.Data.Logger
import HaskellWorks.Prelude hiding (Floating(..))
import System.IO (Handle)
import System.IO qualified as IO

data DataLog (i :: Type) :: Effect

type instance DispatchOf (DataLog i) = Static NoSideEffects

newtype instance StaticRep (DataLog i) = DataLog (Logger i)

runDataLog :: ()
  => r <: IOE
  => HasCallStack
  => UnliftStrategy
  -> (i -> Eff r ())
  -> Eff (DataLog i : r) a
  -> Eff r a
runDataLog strategy run f = do
  s <- mkLogger strategy run
  evalStaticRep (DataLog s) f

runDataLogTextToHandle :: ()
  => HasCallStack
  => Handle
  -> Eff (DataLog Text : r) a
  -> Eff r a
runDataLogTextToHandle h =
  evalStaticRep $ DataLog $ Logger $ T.hPutStrLn h

runDataLogTextToStdout :: ()
  => HasCallStack
  => Eff (DataLog Text : r) a
  -> Eff r a
runDataLogTextToStdout =
  runDataLogTextToHandle IO.stdout

runDataLogTextToStderr :: ()
  => HasCallStack
  => Eff (DataLog Text : r) a
  -> Eff r a
runDataLogTextToStderr =
  runDataLogTextToHandle IO.stderr

withDataLogSerialiser :: ()
  => HasCallStack
  => (Logger i -> Logger o)
  -> Eff (DataLog o : r) a
  -> Eff (DataLog i : r) a
withDataLogSerialiser f m = do
  logger <- getLogger
  let _ = logger
  raise $ evalStaticRep (DataLog (f logger)) m

withDataLog :: ()
  => HasCallStack
  => (o -> i)
  -> Eff (DataLog o : r) a
  -> Eff (DataLog i : r) a
withDataLog =
  withDataLogSerialiser . contramap

getLogger :: ()
  => HasCallStack
  => r <: DataLog i
  => Eff r (Logger i)
getLogger = do
  DataLog i <- getStaticRep
  pure i

log :: ()
  => HasCallStack
  => r <: DataLog i
  => r <: IOE
  => i
  -> Eff r ()
log i = do
  logger <- getLogger
  liftIO $ logger.run i

local :: ()
  => HasCallStack
  => r <: DataLog i
  => (i -> i)
  -> Eff r a
  -> Eff r a
local f =
  localStaticRep $ \(DataLog s) -> DataLog (contramap f s)
