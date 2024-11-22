{- HLINT ignore "Use let" -}

module Effectful.Zoo.Amazonka.Static
  ( Amazonka,
    runAmazonka,
    withAmazonka,
    localAmazonka,

    runReaderAwsEnvDiscover,
    runDataLogAwsLogEntryToLog,
    runDataLogAwsLogEntryToLogWith,
    runDataLogAwsLogEntryLocalToLogWith,

    askAwsEnv,
    sendAws,
  ) where

import Amazonka qualified as AWS
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.ByteString.Builder qualified as B
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as LT
import Data.Time.Clock qualified as IO
import Data.Typeable
import Effectful
import Effectful.Dispatch.Static
import Effectful.Environment
import Effectful.Error.Static
import Effectful.Reader.Static
import Effectful.Zoo.Amazonka.Api
import Effectful.Zoo.Amazonka.Data
import Effectful.Zoo.Core
import Effectful.Zoo.DataLog.Api
import Effectful.Zoo.DataLog.Data.LogEntry
import Effectful.Zoo.DataLog.Dynamic
import Effectful.Zoo.Log.Data.LogMessage
import Effectful.Zoo.Log.Data.Severity
import GHC.Stack qualified as GHC
import HaskellWorks.Prelude

data Amazonka :: Effect

type instance DispatchOf Amazonka = Static WithSideEffects

newtype instance StaticRep Amazonka = Amazonka AwsEnv

runReaderAwsEnvDiscover :: forall a r. ()
  => r <: Environment
  => r <: IOE
  => Eff (Reader AwsEnv : r) a
  -> Eff r a
runReaderAwsEnvDiscover f = do
  awsEnv <- discoverAwsEnv

  runReader awsEnv f

runAmazonka :: forall a r. ()
  => r <: IOE
  => AwsEnv
  -> Eff (Amazonka : r) a
  -> Eff r a
runAmazonka awsEnv =
  evalStaticRep $ Amazonka awsEnv

withAmazonka :: forall r a. ()
  => HasCallStack
  => r <: IOE
  => (AwsEnv -> AwsEnv)
  -> Eff (Amazonka : r) a
  -> Eff (Amazonka : r) a
withAmazonka f m = do
  Amazonka awsEnv <- getStaticRep

  raise $ runAmazonka (f awsEnv) m

localAmazonka :: ()
  => HasCallStack
  => r <: Amazonka
  => (AwsEnv -> AwsEnv)
  -> Eff r a
  -> Eff r a
localAmazonka f =
  localStaticRep $ \(Amazonka r) -> Amazonka (f r)

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
  runDataLog (ConcUnlift Persistent Unlimited) $ \logEntry -> do
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

askAwsEnv :: forall r. ()
  => r <: Amazonka
  => Eff r AwsEnv
askAwsEnv = do
  Amazonka awsEnv <- getStaticRep

  pure awsEnv

sendAws :: forall a r. ()
  => HasCallStack
  => AwsRequest a
  => r <: Amazonka
  => r <: DataLog AwsLogEntry
  => r <: Error AwsError
  => r <: IOE
  => Typeable (AwsResponse a)
  => Typeable a
  => a
  -> Eff r (AwsResponse a)
sendAws req = GHC.withFrozenCallStack $ do
  envAws0 <- askAwsEnv

  envAws1 <- withEffToIO (ConcUnlift Persistent Unlimited) $ \effToIO ->
    pure $ envAws0
      { AWS.logger = \logLevel builder ->
          effToIO $ dataLog $ AwsLogEntry GHC.callStack logLevel builder
      }

  liftIO (runResourceT $ AWS.sendEither envAws1 req)
    & onLeftM throwError
