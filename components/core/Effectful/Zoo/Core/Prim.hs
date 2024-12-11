module Effectful.Zoo.Core.Prim
  ( type (<:)
  ) where

import           Effectful ((:>))

type (<:) r e = (:>) e r
