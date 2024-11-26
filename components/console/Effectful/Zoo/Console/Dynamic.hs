module Effectful.Zoo.Console.Dynamic
  ( Console (..),
    runConsole,
    runConsoleBrackedToConsole,
    runConsoleAtomic,
  ) where

import Effectful
import Effectful.Exception
import Effectful.Concurrent.QSem
import Effectful.Dispatch.Dynamic
import Effectful.Zoo.Core
import Effectful.Zoo.Console.Static qualified as S
import HaskellWorks.Prelude

data Console a :: Effect where
  Print
    :: a
    -> Console a m ()

  Local
    :: (a -> a)
    -> m b
    -> Console a m b

type instance DispatchOf (Console a) = Dynamic

runConsole :: ()
  => HasCallStack
  => r <: IOE
  => UnliftStrategy
  -> (HasCallStack => i -> Eff r ())
  -> Eff (Console i : r) a
  -> Eff r a
runConsole s run =
  reinterpret (S.runConsole s run) $ \env -> \case
    Print i -> S.print i
    Local f m -> localSeqUnlift env $ \unlift -> S.local f (unlift m)

runConsoleBrackedToConsole :: ()
  => HasCallStack
  => r <: Console i
  => Eff r b
  -> Eff r c
  -> Eff (Console i : r) a
  -> Eff r a
runConsoleBrackedToConsole before after =
  interpret $ \env -> \case
    Print i ->
      bracket_ before after $
        send $ Print i
    Local f m ->
      bracket_ before after $
        localSeqUnlift env $ \unlift -> send $ Local f (unlift m)

runConsoleAtomic :: ()
  => HasCallStack
  => r <: Concurrent
  => r <: Console i
  => Eff (Console i : r) a
  -> Eff r a
runConsoleAtomic f = do
  qsem <- newQSem 1
  runConsoleBrackedToConsole (waitQSem qsem) (signalQSem qsem) f
