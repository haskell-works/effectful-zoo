module Effectful.Zoo.DataLog.Data.LogEntry
  ( LogEntry(..),
    annotate,
  ) where

import Data.Time (UTCTime)
import Data.Time.Clock qualified as IO
import Effectful
import Effectful.Zoo.Core
import GHC.Stack qualified as GHC
import HaskellWorks.Prelude

data LogEntry a = LogEntry
  { message :: !a
  , time :: !UTCTime
  , source :: !CallStack
  }
  deriving stock (Generic, Show)

annotate :: ()
  => HasCallStack
  => r <: IOE
  => a
  -> Eff r (LogEntry a)
annotate msg = do
  time <- liftIO IO.getCurrentTime
  pure (LogEntry msg time GHC.callStack)
