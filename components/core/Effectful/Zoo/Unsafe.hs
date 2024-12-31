{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Effectful.Zoo.Unsafe
  ( unsafeFileSystemEff_
  ) where

import Effectful
import Effectful.Dispatch.Static
import Effectful.FileSystem
import Effectful.Zoo.Core
import HaskellWorks.Prelude

unsafeFileSystemEff_ :: ()
  => r <: FileSystem
  => IO a
  -> Eff r a
unsafeFileSystemEff_ =
  unsafeEff_
