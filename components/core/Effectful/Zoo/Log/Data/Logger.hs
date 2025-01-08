module Effectful.Zoo.Log.Data.Logger
  ( Logger(..),
    mkLogger,
  ) where

import Effectful
import Effectful.Zoo.Core
import Effectful.Zoo.Log.Data.LogMessage
import HaskellWorks.Prelude

newtype Logger i = Logger
  { run :: CallStack -> LogMessage i -> IO ()
  } deriving stock Generic

instance Contravariant Logger where
  contramap f (Logger g) =
    Logger \cs m -> g cs (fmap f m)

mkLogger :: ()
  => r <: IOE
  => UnliftStrategy
  -> (CallStack -> LogMessage i -> Eff r ())
  -> Eff r (Logger i)
mkLogger strategy run =
  withEffToIO strategy $ \effToIO ->
    pure $ Logger $ \cs m -> effToIO $ run cs m
