{- HLINT ignore "Use let" -}

module Effectful.Zoo.Amazonka.Static
  ( Amazonka,
    runAmazonka,
    withAmazonka,
    localAmazonka,
    askAwsEnv,
    sendAws,
    lazySendAws,
  ) where

import Amazonka qualified as AWS
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Typeable
import Effectful
import Effectful.Dispatch.Static
import Effectful.Error.Static
import Effectful.Zoo.Amazonka.Data
import Effectful.Zoo.Core
import Effectful.Zoo.DataLog.Api
import Effectful.Zoo.DataLog.Static
import Effectful.Zoo.Lazy.Dynamic
import GHC.Stack qualified as GHC
import HaskellWorks.Prelude

data Amazonka :: Effect

type instance DispatchOf Amazonka = Static WithSideEffects

newtype instance StaticRep Amazonka = Amazonka AwsEnv

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

lazySendAws :: forall a r. ()
  => HasCallStack
  => AwsRequest a
  => r <: DataLog AwsLogEntry
  => r <: Error AwsError
  => r <: IOE
  => r <: Lazy AwsEnv
  => Typeable (AwsResponse a)
  => Typeable a
  => a
  -> Eff r (AwsResponse a)
lazySendAws req = GHC.withFrozenCallStack $ do
  awsEnv <- lazyAsk
  runAmazonka awsEnv (sendAws req)
