module Effectful.Zoo.Log.Data.LogMessage where

import Effectful.Zoo.Log.Data.Severity (Severity)
import HaskellWorks.Prelude

data LogMessage i =
  LogMessage
  { severity :: !Severity
  , message :: i
  }
  deriving stock (Eq, Generic, Show)
