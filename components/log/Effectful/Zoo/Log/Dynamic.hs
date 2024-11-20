module Effectful.Zoo.Log.Dynamic
  ( Log (..),
    runLog,
  ) where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Zoo.Core
import Effectful.Zoo.Log.Data.Severity
import Effectful.Zoo.Log.Static qualified as S
import HaskellWorks.Prelude

data Log i :: Effect where
  Log
    :: HasCallStack
    => Severity
    -> i
    -> Log i m ()

  Local
    :: (i -> i)
    -> m a
    -> Log i m a

type instance DispatchOf (Log a) = Dynamic

runLog :: ()
  => r <: IOE
  => UnliftStrategy
  -> (CallStack -> Severity -> i -> Eff r ())
  -> Eff (Log i : r) a
  -> Eff r a
runLog s run =
  reinterpret (S.runLog s run) $ \env -> \case
    Log severity i -> S.log severity i
    Local f m -> localSeqUnlift env $ \unlift -> S.local f (unlift m)
