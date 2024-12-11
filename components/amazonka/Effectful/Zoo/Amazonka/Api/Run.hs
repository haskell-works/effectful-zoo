{- HLINT ignore "Use let" -}

module Effectful.Zoo.Amazonka.Api.Run
  ( runDataLogAwsLogEntryToLog,
    runDataLogAwsLogEntryToLogWith,
    runDataLogAwsLogEntryLocalToLogWith,
  ) where

import Control.Monad.IO.Class
import Data.ByteString.Builder qualified as B
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as LT
import Data.Time.Clock qualified as IO
import Effectful
import Effectful.Zoo.Amazonka.Api
import Effectful.Zoo.Amazonka.Data
import Effectful.Zoo.Core
import Effectful.Zoo.DataLog.Api
import Effectful.Zoo.DataLog.Data.LogEntry
import Effectful.Zoo.DataLog.Dynamic
import Effectful.Zoo.Log.Data.LogMessage
import Effectful.Zoo.Log.Data.Severity
import HaskellWorks.Prelude

runDataLogAwsLogEntryToLog :: forall a r. ()
  => r <: DataLog (LogEntry (LogMessage Text))
  => r <: IOE
  => Eff (DataLog AwsLogEntry : r) a
  -> Eff r a
runDataLogAwsLogEntryToLog =
  runDataLogAwsLogEntryToLogWith awsLogLevelToSeverity

runDataLogAwsLogEntryToLogWith :: forall a r. ()
  => r <: DataLog (LogEntry (LogMessage Text))
  => r <: IOE
  => (AwsLogLevel -> Severity)
  -> Eff (DataLog AwsLogEntry : r) a
  -> Eff r a
runDataLogAwsLogEntryToLogWith mapSeverity
  = runDataLogAwsLogEntryLocalToLogWith mapSeverity id

runDataLogAwsLogEntryLocalToLogWith :: forall a r. ()
  => r <: DataLog (LogEntry (LogMessage Text))
  => r <: IOE
  => (AwsLogLevel -> Severity)
  -> (AwsLogEntry -> AwsLogEntry)
  -> Eff (DataLog AwsLogEntry : r) a
  -> Eff r a
runDataLogAwsLogEntryLocalToLogWith mapSeverity context =
  runDataLog $ \logEntry -> do
    now <- liftIO IO.getCurrentTime

    let entry = context logEntry
    let message =
          LogMessage (mapSeverity entry.logLevel) $
            LT.toStrict (LT.decodeUtf8 (B.toLazyByteString entry.builder))

    dataLog $
      LogEntry
        { message = message
        , time = now
        , source = entry.callStack
        }
