module Effectful.Zoo.Log.Dynamic.Effect
  ( Log (..),
    runLog,
  ) where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Zoo.Core
import Effectful.Zoo.Log.Data.LogMessage
import Effectful.Zoo.Log.Static qualified as S
import HaskellWorks.Prelude

data Log i :: Effect where
  Log
    :: HasCallStack
    => LogMessage i
    -> Log i m ()

  Local
    :: (i -> i)
    -> m a
    -> Log i m a

type instance DispatchOf (Log a) = Dynamic

runLog :: forall i a r. ()
  => r <: IOE
  => UnliftStrategy
  -> (CallStack -> LogMessage i -> Eff r ())
  -> Eff (Log i : r) a
  -> Eff r a
runLog s run =
  reinterpret (S.runLog s run) $ \env -> \case
    Log m -> S.log m
    Local f m -> localSeqUnlift env $ \unlift -> S.localLog f (unlift m)
