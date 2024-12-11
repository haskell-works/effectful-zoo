module Effectful.Zoo.Lazy.Dynamic
  ( Lazy(..),
    runLazy,
    lazyAsk,
  ) where

import Effectful
import Effectful.Concurrent.MVar
import Effectful.Dispatch.Dynamic
import Effectful.Zoo.Core
import HaskellWorks.Prelude

data Lazy i :: Effect where
  LazyAsk
    :: Lazy i m i

type instance DispatchOf (Lazy i) = Dynamic

runLazy :: forall i a r. ()
  => r <: Concurrent
  => Eff r i
  -> Eff (Lazy i : r) a
  -> Eff r a
runLazy f h = do
  mvCache <- newMVar @_ @(Maybe i) Nothing
  interpret
    do \_ -> \case
        LazyAsk -> do
          resultOrRunning <- takeMVar mvCache
          case resultOrRunning of
            Just result -> do
              putMVar mvCache (Just result)
              return result
            Nothing -> do
              result <- f
              putMVar mvCache (Just result)
              return result
    do h

lazyAsk :: ()
  => r <: Lazy i
  => Eff r i
lazyAsk =
  send LazyAsk