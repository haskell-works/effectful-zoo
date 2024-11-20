module Effectful.Zoo.Core
  ( type (<:)
  ) where

import           Effectful ((:>))

type (<:) r e = (:>) e r
