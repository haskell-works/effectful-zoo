module Effectful.Zoo.Amazonka.Api.Log
  ( awsLogLevelToSeverity,
  ) where

import Amazonka qualified as AWS
import Effectful.Zoo.Amazonka.Data
import Effectful.Zoo.Log.Data.Severity

awsLogLevelToSeverity :: ()
  => AwsLogLevel
  -> Severity
awsLogLevelToSeverity = \case
  AWS.Trace -> Trace
  AWS.Debug -> Debug
  AWS.Info  -> Info
  AWS.Error -> Error
