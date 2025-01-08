module Effectful.Zoo.DataLog.Api
  ( dataLog,
    logEntryToJson,
    logMessageToJson,
  ) where

import Data.Aeson (Value, object, (.=))
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Zoo.Core
import Effectful.Zoo.DataLog.Data.LogEntry
import Effectful.Zoo.DataLog.Dynamic
import Effectful.Zoo.Log.Data.LogMessage
import GHC.Stack qualified as GHC
import HaskellWorks.Prelude

dataLog :: ()
  => HasCallStack
  => r <: DataLog i
  => i
  -> Eff r ()
dataLog i =
  withFrozenCallStack do
    send $ DataLog i

logEntryToJson :: forall a. ()
  => (a -> Value)
  -> LogEntry a
  -> Value
logEntryToJson aToJson (LogEntry value time callstack) =
    object
      [ "time" .= time
      , "data" .= aToJson value
      , "callstack" .= fmap callsiteToJson (GHC.getCallStack callstack)
      ]
    where
      callsiteToJson :: ([Char], GHC.SrcLoc) -> Value
      callsiteToJson (caller, srcLoc) =
        object
          [ "caller"    .= caller
          , "package"   .= GHC.srcLocPackage srcLoc
          , "module"    .= GHC.srcLocModule srcLoc
          , "file"      .= GHC.srcLocFile srcLoc
          , "startLine" .= GHC.srcLocStartLine srcLoc
          , "startCol"  .= GHC.srcLocStartCol srcLoc
          , "endLine"   .= GHC.srcLocEndLine srcLoc
          , "endCol"    .= GHC.srcLocEndCol srcLoc
          ]

logMessageToJson :: LogMessage Text -> Value
logMessageToJson (LogMessage severity message) =
    object
      [ "severity" .= show severity
      , "message"  .= message
      ]
