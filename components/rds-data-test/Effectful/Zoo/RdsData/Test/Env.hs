{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

{- HLINT ignore "Redundant pure" -}
{- HLINT ignore "Use let" -}

module Effectful.Zoo.RdsData.Test.Env
  ( AwsResourceArn(..),
    AwsSecretArn(..),
    runLocalTestEnv,
    runTestEnv,
    runReaderFromEnvOrFail,
    runReaderStatementContextFromClusterDetails,
  ) where

import Amazonka qualified as AWS
import Data.Generics.Product.Any
import Data.RdsData.Aws
import Data.RdsData.Migration.Types
import Effectful
import Effectful.Concurrent
import Effectful.Zoo.Amazonka.Api.Run
import Effectful.Zoo.Core
import Effectful.Zoo.Environment
import Effectful.Zoo.Error.Static
import Effectful.Zoo.Hedgehog.Api
import Effectful.Zoo.Hedgehog.Effect.Hedgehog
import Effectful.Zoo.Reader.Static
import Effectful.Zoo.TestContainers.LocalStack
import HaskellWorks.Prelude
import Lens.Micro

runTestEnv :: ()
  => HasCallStack
  => r <: Concurrent
  => r <: Environment
  => r <: Error Failure
  => r <: Hedgehog
  => r <: IOE
  => Eff
        ( Reader AWS.Env
        : Reader AwsResourceArn
        : Reader AwsSecretArn
        : r)
      a
  -> Eff r a
runTestEnv f =
  withFrozenCallStack $ f
    & runReaderAwsEnvDiscover
    & runReaderFromEnvOrFail AwsResourceArn "AURORA_RESOURCE_ARN"
    & runReaderFromEnvOrFail AwsSecretArn "AURORA_SECRET_ARN"

runLocalTestEnv :: ()
  => HasCallStack
  => r <: IOE
  => IO Container
  -> Eff
        ( Reader AWS.Env
        : r)
      a
  -> Eff r a
runLocalTestEnv getContainer f =
  withFrozenCallStack $ f
    & runReaderLocalAwsEnvDiscover getContainer

runReaderFromEnvOrFail :: forall i r a. ()
  => r <: Concurrent
  => r <: Environment
  => r <: Error Failure
  => r <: Hedgehog
  => (Text -> i)
  -> Text
  -> Eff (Reader i ': r) a
  -> Eff r a
runReaderFromEnvOrFail f envVar action = do
  env <- lookupEnv envVar
    & trapFail

  runReader (f env) action

runReaderStatementContextFromClusterDetails :: ()
  => r <: Concurrent
  => r <: Error Failure
  => r <: Hedgehog
  => RdsClusterDetails
  -> Eff (Reader StatementContext : r) a
  -> Eff r a
runReaderStatementContextFromClusterDetails details f = do
  resourceArn <- (details ^. the @"createDbClusterResponse" . the @"dbCluster" . _Just . the @"dbClusterArn")
    & onNothingFail

  secretArn <- (details ^. the @"createSecretResponse" ^. the @"arn")
    & onNothingFail

  mDatabase <- pure $ (details ^? the @"createDbClusterResponse" . the @"dbCluster" . _Just . the @"databaseName" . _Just)
    <&> Database

  statementContext <- pure $ newStatementContext (AwsResourceArn resourceArn) (AwsSecretArn secretArn)
    & the @"database" .~ mDatabase


  f & runReader statementContext
