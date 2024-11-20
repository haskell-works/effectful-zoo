module Effectful.Zoo.Log.Data.LogMessage where

import Effectful.Zoo.Log.Data.Severity (Severity)
import HaskellWorks.Prelude

data LogMessage =
  LogMessage
  { severity :: !Severity
  , message :: Text
  }
  deriving stock (Eq, Generic, Show)
