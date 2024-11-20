module Effectful.Zoo.Log.Data.Logger
  ( Logger(..),
    mkLogger,
  ) where

import Effectful
import Effectful.Zoo.Core
import Effectful.Zoo.Log.Data.Severity
import HaskellWorks.Prelude hiding (Floating(..))

newtype Logger i = Logger
  { run :: CallStack -> Severity -> i -> IO ()
  } deriving stock Generic

instance Contravariant Logger where
  contramap f (Logger g) = Logger \cs severity -> g cs severity . f

mkLogger :: ()
  => r <: IOE
  => UnliftStrategy
  -> (CallStack -> Severity -> i -> Eff r ())
  -> Eff r (Logger i)
mkLogger strategy run =
  withEffToIO strategy $ \effToIO ->
    pure $ Logger $ \cs severity i -> effToIO $ run cs severity i
