module Effectful.Zoo.Log.Effect
  ( Log(..),
  ) where

import Effectful
import Effectful.Zoo.Log.Data.LogMessage
import HaskellWorks.Prelude

data Log :: Effect where
  Log
    :: HasCallStack
    => LogMessage
    -> Log m ()

type instance DispatchOf Log = Dynamic
