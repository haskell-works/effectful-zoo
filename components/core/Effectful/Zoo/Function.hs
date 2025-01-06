module Effectful.Zoo.Function
  ( once,
  ) where

import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.MVar
import Effectful.Zoo.Prim
import HaskellWorks.Prelude

once :: ()
  => r <: Concurrent
  => Eff r a
  -> Eff r (Eff r a)
once f = do
  cache <- newMVar Nothing
  return $ do
    resultOrRunning <- takeMVar cache
    case resultOrRunning of
      Just result -> do
        putMVar cache (Just result)
        return result
      Nothing -> do
        result <- f
        putMVar cache (Just result)
        return result
