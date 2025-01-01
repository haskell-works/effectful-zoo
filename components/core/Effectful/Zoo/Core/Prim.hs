module Effectful.Zoo.Core.Prim
  ( type (<:),
    type (<<:)
  ) where

import           Effectful ((:>), (:>>))

type (<:) r e = (:>) e r

type (<<:) r e = (:>>) e r
