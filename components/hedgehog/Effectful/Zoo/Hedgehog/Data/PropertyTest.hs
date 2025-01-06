module Effectful.Zoo.Hedgehog.Data.PropertyTest
  ( PropertyTest,
  ) where

import Hedgehog
import HaskellWorks.Prelude

type PropertyTest = PropertyT IO ()
