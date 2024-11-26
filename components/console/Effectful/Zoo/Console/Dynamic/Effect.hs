module Effectful.Zoo.Console.Dynamic.Effect
  ( Console (..),
  ) where

import Effectful

data Console a :: Effect where
  Print
    :: a
    -> Console a m ()

  Local
    :: (a -> a)
    -> m b
    -> Console a m b

type instance DispatchOf (Console a) = Dynamic
