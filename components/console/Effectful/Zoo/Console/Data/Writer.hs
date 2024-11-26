module Effectful.Zoo.Console.Data.Writer
  ( Writer(..),
    mkWriter,
  ) where

import Effectful
import Effectful.Zoo.Core
import HaskellWorks.Prelude

newtype Writer i = Writer
  { run :: i -> IO ()
  } deriving stock Generic

instance Contravariant Writer where
  contramap f (Writer g) = Writer (g . f)

mkWriter :: ()
  => r <: IOE
  => UnliftStrategy
  -> (i -> Eff r ())
  -> Eff r (Writer i)
mkWriter strategy run =
  withEffToIO strategy $ \effToIO ->
    pure $ Writer $ effToIO . run
