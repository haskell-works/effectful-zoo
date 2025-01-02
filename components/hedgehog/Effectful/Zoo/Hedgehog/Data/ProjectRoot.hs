module Effectful.Zoo.Hedgehog.Data.ProjectRoot
  ( ProjectRoot(..),
  ) where

import           GHC.Generics
import           HaskellWorks.Prelude

newtype ProjectRoot = ProjectRoot
  { filePath :: FilePath
  }
  deriving stock (Eq, Generic, Show)
