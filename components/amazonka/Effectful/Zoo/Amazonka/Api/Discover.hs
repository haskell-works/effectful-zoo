{- HLINT "Use let" -}

module Effectful.Zoo.Amazonka.Api.Discover
  ( discoverAwsEnv,
    maybeSetEndpoint,
  ) where

import Amazonka qualified as AWS
import Control.Monad.IO.Class
import Data.Generics.Product.Any
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Effectful
import Effectful.Environment
import Effectful.Zoo.Amazonka.Data
import Effectful.Zoo.Core
import HaskellWorks.Prelude
import Lens.Micro
import System.IO qualified as IO
import Text.Read

maybeSetEndpoint :: ()
  => Maybe (String, String)
  -> (AwsService -> AwsService)
  -> AwsService
  -> AwsService
maybeSetEndpoint = \case
  Just (host, portString) ->
    case readMaybe portString of
      Just port -> (. AWS.setEndpoint False (T.encodeUtf8 (T.pack host)) port)
      Nothing   -> id
  Nothing           -> id

discoverAwsEnv :: ()
  => r <: Environment
  => r <: IOE
  => Eff r AwsEnv
discoverAwsEnv = do
  logger' <- liftIO $ AWS.newLogger AWS.Debug IO.stdout

  mLocalStackHost <- lookupEnv "AWS_LOCALSTACK_HOST"
  mLocalStackPort <- lookupEnv "AWS_LOCALSTACK_PORT"

  mLocalStackEndpoint <- pure $ (,)
    <$> mLocalStackHost
    <*> mLocalStackPort

  discoveredAwsEnv <- liftIO $ AWS.newEnv AWS.discover

  pure $ discoveredAwsEnv
    & the @"logger" .~ logger'
    & the @"overrides" %~ maybeSetEndpoint mLocalStackEndpoint
  