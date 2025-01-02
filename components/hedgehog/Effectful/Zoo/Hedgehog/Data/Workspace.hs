module Effectful.Zoo.Hedgehog.Data.Workspace
  ( Workspace(..),
  ) where

import           GHC.Generics
import           HaskellWorks.Prelude

newtype Workspace = Workspace
  { filePath :: FilePath
  }
  deriving stock (Eq, Generic, Show)
