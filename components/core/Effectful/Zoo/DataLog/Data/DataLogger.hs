module Effectful.Zoo.DataLog.Data.DataLogger
  ( DataLogger(..),
    mkDataLogger,
  ) where

import Effectful
import Effectful.Dispatch.Static
import HaskellWorks.Prelude

newtype DataLogger i = DataLogger
  { run :: i -> IO ()
  } deriving stock Generic

instance Contravariant DataLogger where
  contramap f (DataLogger g) = DataLogger (g . f)

mkDataLogger :: ()
  => (i -> Eff r ())
  -> Eff r (DataLogger i)
mkDataLogger run =
  unsafeConcUnliftIO Persistent Unlimited $ \effToIO ->
    pure $ DataLogger $ effToIO . run
