module Effectful.Zoo.Hedgehog.Api.Internal.FilePath
  ( exeSuffix,
    addExeSuffix,
  ) where

import Data.List qualified as L
import Effectful.Zoo.Hedgehog.Api.Internal.OS qualified as OS
import HaskellWorks.Prelude

exeSuffix :: String
exeSuffix = if OS.isWin32 then ".exe" else ""

addExeSuffix :: String -> String
addExeSuffix s = if ".exe" `L.isSuffixOf` s
  then s
  else s <> exeSuffix
