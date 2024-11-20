module Effectful.Zoo.DataLog.Data.Logger
  ( Logger(..),
    mkLogger,
  ) where

import Effectful
import Effectful.Zoo.Core
import HaskellWorks.Prelude hiding (Floating(..))

newtype Logger i = Logger
  { run :: i -> IO ()
  } deriving stock Generic

instance Contravariant Logger where
  contramap f (Logger g) = Logger (g . f)

mkLogger :: ()
  => r <: IOE
  => UnliftStrategy
  -> (i -> Eff r ())
  -> Eff r (Logger i)
mkLogger strategy run =
  withEffToIO strategy $ \effToIO ->
    pure $ Logger $ effToIO . run
