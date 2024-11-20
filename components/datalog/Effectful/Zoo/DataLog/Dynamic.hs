module Effectful.Zoo.DataLog.Dynamic
  ( DataLog (..),
    runDataLog,
  ) where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Zoo.Core
import Effectful.Zoo.DataLog.Static qualified as S
import HaskellWorks.Prelude

data DataLog a :: Effect where
  DataLog
    :: a
    -> DataLog a m ()

  Local
    :: (a -> a)
    -> m b
    -> DataLog a m b

type instance DispatchOf (DataLog a) = Dynamic

runDataLog :: ()
  => HasCallStack
  => r <: IOE
  => UnliftStrategy
  -> (HasCallStack => i -> Eff r ())
  -> Eff (DataLog i : r) a
  -> Eff r a
runDataLog s run =
  reinterpret (S.runDataLog s run) $ \env -> \case
    DataLog i -> S.log i
    Local f m -> localSeqUnlift env $ \unlift -> S.local f (unlift m)
