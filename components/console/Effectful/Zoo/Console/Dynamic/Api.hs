module Effectful.Zoo.Console.Dynamic.Api
  ( Console (..),
    print,
  ) where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Zoo.Console.Dynamic.Effect
import Effectful.Zoo.Core
import HaskellWorks.Prelude

print :: ()
  => HasCallStack
  => r <: Console i
  => i
  -> Eff r ()
print i =
  send $ Print i
