module Effectful.Zoo.Hedgehog.Data.UnitTest
  ( UnitTest,
  ) where

import Hedgehog
import HaskellWorks.Prelude

type UnitTest = TestT IO ()
