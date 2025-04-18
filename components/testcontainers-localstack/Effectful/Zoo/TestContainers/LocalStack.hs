{- HLINT ignore "Use camelCase" -}

module Effectful.Zoo.TestContainers.LocalStack (
    LocalStackEndpoint (..),
    TC.Container,
    setupContainers,
    setupContainers',
    waitForLocalStack,
    runReaderLocalAwsEnvDiscover,
    getLocalStackEndpoint,
    inspectContainer,
) where

import Amazonka qualified as AWS
import Amazonka.Auth qualified as AWS
import Control.Concurrent (threadDelay)
import Control.Concurrent qualified as IO
import Control.Exception (try)
import Control.Exception qualified as E
import Control.Monad.Catch (catch)
import Control.Monad.IO.Class
import Data.Aeson qualified as J
import Data.ByteString.Lazy qualified as LBS
import Data.Function
import Data.Generics.Product.Any
import Data.Time.Clock.POSIX (getPOSIXTime)
import Effectful
import Effectful.Zoo.Core
import Effectful.Zoo.Reader.Static
import Effectful.Zoo.TestContainers.LocalStack.Types (LocalStackEndpoint (LocalStackEndpoint))
import Effectful.Zoo.TestContainers.LocalStack.Types qualified as Z
import HaskellWorks.Prelude
import Lens.Micro
import Network.HTTP.Conduit (HttpException, simpleHttp)
import System.Environment qualified as IO
import System.IO qualified as IO
import TestContainers.Monad qualified as TC
import TestContainers.Tasty qualified as TC
import Prelude

-- | Sets up and runs the containers required for this test suite.
setupContainers ::
    () =>
    (TC.MonadDocker m) =>
    m TC.Container
setupContainers = setupContainers' "localstack/localstack-pro:latest"

-- | Sets up and runs the containers required for this test suite.
setupContainers' ::
    () =>
    (TC.MonadDocker m) =>
    Text ->
    m TC.Container
setupContainers' dockerTag = do
    _authToken <- liftIO $ IO.lookupEnv "LOCALSTACK_AUTH_TOKEN"
    -- Launch the container based on the postgres image.
    localstackContainer <-
        ( TC.run $
            TC.containerRequest (TC.fromTag dockerTag)
                & TC.setEnv [("ACTIVATE_PRO", "0")]
                -- Expose the port 4566 from within the container. The respective port
                -- on the host machine can be looked up using `containerPort` (see below).
                & TC.setExpose
                    ( mconcat
                        [ [4566]
                        ]
                    )
                -- Wait until the container is ready to accept requests. `run` blocks until
                -- readiness can be established.
                & TC.setWaitingFor (TC.waitUntilTimeout 60 (TC.waitForHttp 4566 "http://localhost:4566/health" [200]))
                --  & (TC.waitUntilTimeout 30 (TC.waitUntilMappedPortReachable 4566))
        )
            `catch` (\(e :: SomeException) -> error $ "run: " <> show e)

    -- Look up the corresponding port on the host machine for the exposed port 4566.
    let localStackPort = TC.containerPort localstackContainer 4566

    liftIO $ waitForLocalStack "localhost" localStackPort 100 `catch` (\(e :: SomeException) -> error $ "waitForLocalStack: " <> show e)

    pure localstackContainer

waitForLocalStack :: String -> Int -> Int -> IO ()
waitForLocalStack host port timeout = do
    startTime <- getPOSIXTime
    let url = "http://" <> host <> ":" <> show port
    checkLoop startTime url
  where
    checkLoop startTime url = do
        result <- try $ simpleHttp url :: IO (Either HttpException LBS.ByteString)
        case result of
            Right _ -> do
                IO.threadDelay 1_000_000
                IO.putStrLn ""
            Left e -> do
                currentTime <- getPOSIXTime
                let elapsedTime = currentTime - startTime
                when (elapsedTime < fromIntegral timeout) $ do
                    threadDelay 500_000
                    checkLoop startTime url
                when (elapsedTime >= fromIntegral timeout) $ do
                    IO.putStrLn "Timeout reached. LocalStack is not ready."
                    E.throw e

runReaderLocalAwsEnvDiscover ::
    forall a r.
    () =>
    (r <: IOE) =>
    IO TC.Container ->
    Eff (Reader AWS.Env : r) a ->
    Eff r a
runReaderLocalAwsEnvDiscover mk f = do
    container <- liftIO mk
    ep <- getLocalStackEndpoint container

    logger' <- liftIO $ AWS.newLogger AWS.Debug IO.stdout

    let creds = AWS.fromKeys (AWS.AccessKey "test") (AWS.SecretKey "test")

    credEnv <- liftIO $ AWS.newEnv (AWS.runCredentialChain [pure . creds])

    awsEnv <-
        pure $
            credEnv
                & the @"logger" .~ logger'
                & the @"overrides" %~ (. AWS.setEndpoint False "localhost" ep.port)

    runReader awsEnv f

getLocalStackEndpoint ::
    () =>
    TC.Container ->
    Eff r LocalStackEndpoint
getLocalStackEndpoint container = do
    let localStackPort = TC.containerPort container 4566

    pure
        Z.LocalStackEndpoint
            { Z.host = "0.0.0.0"
            , Z.port = localStackPort
            }

inspectContainer ::
    () =>
    (r <: IOE) =>
    TC.Container ->
    Eff r J.Value
inspectContainer container =
    liftIO $ TC.runTestContainer TC.defaultDockerConfig $ TC.inspect container
