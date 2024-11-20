module Effectful.Zoo.Log.Data.Severity
  ( Severity (..),
    parseSeverity,
  ) where

import GHC.Enum
import Data.Text qualified as T
import HaskellWorks.Prelude

data Severity =
    Trace
  | Debug
  | Info
  | Warn
  | Error
  | Crit
  deriving stock (Enum, Eq, Generic, Ord, Show)

parseSeverity :: Text -> Maybe Severity
parseSeverity t =
  case T.toLower t of
    "trace" -> Just Trace
    "debug" -> Just Debug
    "info" -> Just Info
    "warn" -> Just Warn
    "error" -> Just Error
    "crit" -> Just Crit
    _ -> Nothing
