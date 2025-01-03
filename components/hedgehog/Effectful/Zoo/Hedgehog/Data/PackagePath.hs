module Effectful.Zoo.Hedgehog.Data.PackagePath
  ( PackagePath(..),
  ) where

import           GHC.Generics
import           HaskellWorks.Prelude

newtype PackagePath = PackagePath
  { filePath :: FilePath
  }
  deriving stock (Eq, Generic, Show)
