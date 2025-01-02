module Effectful.Zoo.Hedgehog.Api.Stack
  ( callerModuleName,
  ) where

import GHC.Stack (callStack, getCallStack, srcLocModule)
import HaskellWorks.Prelude

-- | Get the module name of the caller.
callerModuleName :: HasCallStack => String
callerModuleName = maybe "<no-module>" (srcLocModule . snd) (listToMaybe (getCallStack callStack))
