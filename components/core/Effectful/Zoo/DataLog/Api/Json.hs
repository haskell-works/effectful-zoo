module Effectful.Zoo.DataLog.Api.Json
  ( logEntryToJson,
    logMessageToJson,
    putJsonStdout,
  ) where

import Data.Aeson (Value, object, (.=))
import Data.Aeson qualified as J
import Data.ByteString.Lazy qualified as LBS
import Effectful
import Effectful.Dispatch.Static
import Effectful.Zoo.DataLog.Data.LogEntry
import Effectful.Zoo.Log.Data.LogMessage
import GHC.Stack qualified as GHC
import HaskellWorks.Prelude
import System.IO qualified as IO

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

putJsonStdout :: ()
  => Value
  -> Eff r ()
putJsonStdout value = do
  unsafeEff_ $ LBS.putStr $ J.encode value <> "\n"
  unsafeEff_ $ IO.hFlush IO.stdout
