module Effectful.Zoo.Environment
  ( -- * Effect
    Environment,

    -- ** Handlers
    E.runEnvironment,

    -- * Querying the environment
    E.getArgs,
    E.getProgName,
    E.getExecutablePath,
    E.getEnv,
    E.getEnvironment,
    lookupEnv,
    lookupEnvMaybe,

    -- * Modifying the environment
    E.setEnv,
    E.unsetEnv,
    E.withArgs,
    E.withProgName,
  )
  where

import Data.Text qualified as T
import Effectful
import Effectful.Environment (Environment)
import Effectful.Environment qualified as E
import Effectful.Zoo.Core
import Effectful.Zoo.Error.Static
import Effectful.Zoo.Errors.EnvironmentVariableMissing
import HaskellWorks.Prelude

lookupEnv :: ()
  => r <: Environment
  => r <: Error EnvironmentVariableMissing
  => Text
  -> Eff r Text
lookupEnv envName =
  lookupEnvMaybe envName
    & onNothingM (throw $ EnvironmentVariableMissing envName)

lookupEnvMaybe :: ()
  => r <: Environment
  => Text
  -> Eff r (Maybe Text)
lookupEnvMaybe envName = do
  value <- E.lookupEnv $ T.unpack envName
  pure $ T.pack <$> value
