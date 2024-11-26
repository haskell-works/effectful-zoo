module Effectful.Zoo.Amazonka.Api.Send
  ( sendAws,
    askAwsEnv,
    localAwsEnv,
  ) where

import Amazonka qualified as AWS
import Effectful
import Effectful.Error.Static
import Effectful.Dispatch.Dynamic
import Effectful.Zoo.Amazonka.Data.AwsError
import Effectful.Zoo.Amazonka.Data.AwsLogEntry
import Effectful.Zoo.Amazonka.Data.AwsRequest
import Effectful.Zoo.Amazonka.Data.AwsResponse
import Effectful.Zoo.Amazonka.Dynamic
import Effectful.Zoo.Core
import Effectful.Zoo.DataLog.Api
import Effectful.Zoo.DataLog.Dynamic
import GHC.Stack qualified as GHC
import HaskellWorks.Prelude

sendAwsInternal :: forall a r. ()
  => AwsRequest a
  => HasCallStack
  => r <: Amazonka
  => r <: Error AwsError
  => Typeable (AwsResponse a)
  => Typeable a
  => a
  -> Eff r (AwsResponse a)
sendAwsInternal req =
  send (SendAws req)
    & onLeftM throwError

askAwsEnv :: ()
  => HasCallStack
  => r <: Amazonka
  => Eff r AWS.Env
askAwsEnv =
  send AskAwsEnv

localAwsEnv :: ()
  => HasCallStack
  => r <: Amazonka
  => (AWS.Env -> AWS.Env)
  -> Eff r a
  -> Eff r a
localAwsEnv f m =
  send (LocalAwsEnv f m)

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
sendAws req = withFrozenCallStack $ do
  envAws0 <- askAwsEnv

  envAws1 <- withEffToIO (ConcUnlift Persistent Unlimited) $ \effToIO ->
    pure $ envAws0
      { AWS.logger = \logLevel builder ->
          effToIO $ dataLog $ AwsLogEntry GHC.callStack logLevel builder
      }

  localAwsEnv (const envAws1) $
    sendAwsInternal req
