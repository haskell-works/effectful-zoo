module Effectful.Zoo.DataLog.Data.DataLogger
  ( DataLogger(..),
    mkDataLogger,
  ) where

import Effectful
import Effectful.Zoo.Core
import HaskellWorks.Prelude hiding (Floating(..))

newtype DataLogger i = DataLogger
  { run :: i -> IO ()
  } deriving stock Generic

instance Contravariant DataLogger where
  contramap f (DataLogger g) = DataLogger (g . f)

mkDataLogger :: ()
  => r <: IOE
  => UnliftStrategy
  -> (i -> Eff r ())
  -> Eff r (DataLogger i)
mkDataLogger strategy run =
  withEffToIO strategy $ \effToIO ->
    pure $ DataLogger $ effToIO . run
