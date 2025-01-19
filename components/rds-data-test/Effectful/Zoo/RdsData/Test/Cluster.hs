{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{- HLINT ignore "Redundant id" -}
{- HLINT ignore "Redundant pure" -}
{- HLINT ignore "Use let" -}

module Effectful.Zoo.RdsData.Test.Cluster
  ( RdsClusterDetails(..),
    createRdsDbCluster,
    waitUntilRdsDbClusterAvailable,
  ) where

import Amazonka qualified as AWS
import Amazonka.RDS qualified as AWS
import Amazonka.SecretsManager qualified as AWS
import Data.Aeson ((.=))
import Data.Aeson qualified as J
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Lazy qualified as LBS
import Data.Function
import Data.Generics.Product.Any
import Data.RdsData.Migration.Types (RdsClusterDetails (RdsClusterDetails))
import Data.Text.Encoding qualified as T
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Effectful
import Effectful.Concurrent
import Effectful.Zoo.Amazonka.Api.Send
import Effectful.Zoo.Amazonka.Data.AwsError
import Effectful.Zoo.Amazonka.Data.AwsLogEntry
import Effectful.Zoo.Amazonka.Dynamic
import Effectful.Zoo.Core
import Effectful.Zoo.DataLog.Static
import Effectful.Zoo.Error.Static
import Effectful.Zoo.Hedgehog.Api.Assert
import Effectful.Zoo.Hedgehog.Api.Failure
import Effectful.Zoo.Hedgehog.Api.Journal
import Effectful.Zoo.Hedgehog.Effect.Hedgehog
import Effectful.Zoo.TestContainers.LocalStack
import HaskellWorks.Control.Monad
import HaskellWorks.Prelude
import Lens.Micro

createRdsDbCluster :: ()
  => HasCallStack
  => r <: Amazonka
  => r <: Concurrent
  => r <: Error Failure
  => r <: Hedgehog
  => r <: IOE
  => Text
  -> IO Container
  -> Eff r RdsClusterDetails
createRdsDbCluster databaseName getContainer = withFrozenCallStack do
  container <- liftIO getContainer
  jotShowM_ $ getLocalStackEndpoint container
  jotYamlM_ $ inspectContainer container
  masterUsername <- pure "masterUsername"
  masterPassword <- pure "masterPassword"

  let dbClusterId = "my-cluster"

  createDbClusterRequest <-
    pure $
      AWS.newCreateDBCluster dbClusterId "aurora-postgresql"
        & the @"masterUsername" .~ Just masterUsername
        & the @"masterUserPassword" .~ Just masterPassword
        & the @"enableHttpEndpoint" .~ Just True
        & the @"databaseName" .~ Just databaseName

  createDbClusterResponse <-
    sendAws createDbClusterRequest
      & trapFail @AwsError
      & jotShowDataLog @AwsLogEntry

  let secretName = "my-aurora-cluster"

  secretString <-
    jotYaml $
      J.object
        [ "engine" .= id @Text "aurora-postgresql"
        , "username" .= id @Text masterUsername
        , "password" .= id @Text masterPassword
        , "host" .= id @Text "localhost"
        , "dbname" .= id @Text databaseName
        , "port" .= id @Text "4510"
        ]

  uuid <- liftIO UUID.nextRandom

  let clientRequestToken = T.encodeUtf8 $ UUID.toText uuid

  let secretStringText = T.decodeUtf8 $ LBS.toStrict $ J.encode secretString

  createSecretReq <-
    pure $
      AWS.newCreateSecret secretName
        & the @"secretString" ?~ AWS.Sensitive secretStringText
        & the @"clientRequestToken" ?~ T.decodeUtf8 (B64.encode clientRequestToken)

  createSecetResp <-
    sendAws createSecretReq
      & trapFail
      & jotShowDataLog @AwsLogEntry

  createDbInstanceReq <-
    pure $
      AWS.newCreateDBInstance dbClusterId "my-db-instance" "db.t3.medium"
        & the @"engine" .~ "aurora-postgresql"

  _dbInstanceIdResp <-
    jotShowM $
      sendAws createDbInstanceReq
        & trapFail
        & jotShowDataLog @AwsLogEntry

  pure (RdsClusterDetails createDbClusterResponse createSecetResp)

waitUntilRdsDbClusterAvailable :: ()
  => HasCallStack
  => r <: Amazonka
  => r <: Concurrent
  => r <: DataLog AwsLogEntry
  => r <: Error AWS.Error
  => r <: IOE
  => Text
  -> Eff r ()
waitUntilRdsDbClusterAvailable dbClusterArn =
  withFrozenCallStack do
    repeatNWhileM_ 120 $ \_ -> do
      result <- sendAws $
        AWS.newDescribeDBClusters
          & the @"dbClusterIdentifier" .~ Just dbClusterArn

      let mStatus = result ^? the @"dbClusters" . _Just . each . the @"status" . _Just

      if mStatus == Just "available"
        then threadDelay 1_000_000 >> pure False
        else threadDelay 1_000_000 >> pure True
