module Effectful.Zoo.Amazonka.Data.AwsLogEntry
  ( AwsLogEntry(..),
  ) where

import Data.ByteString.Builder
import Effectful.Zoo.Amazonka.Data.AwsLogLevel
import HaskellWorks.Prelude

data AwsLogEntry = AwsLogEntry
  { callStack :: CallStack
  , logLevel  :: AwsLogLevel
  , builder   :: Builder
  }
  deriving stock Generic
  deriving stock Show
