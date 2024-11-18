module Effectful.Zoo.Hedgehog.Test.HedgehogTest where

import Effectful.Zoo.Hedgehog
import HaskellWorks.Prelude

test_simple :: UnitTest
test_simple =
  runHedgehog do
    jot_ "This is a test"

    True === True
