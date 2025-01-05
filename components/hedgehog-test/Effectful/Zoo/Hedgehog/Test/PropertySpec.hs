{- HLINT ignore "Use camelCase" -}

module Effectful.Zoo.Hedgehog.Test.PropertySpec where

import Effectful.Zoo.Hedgehog.Effect.Run
import HaskellWorks.Prelude
import Hedgehog hiding (property, forAll)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as G
import Hedgehog.Range qualified as R

property_spec :: H.PropertyT IO ()
property_spec = property do
  a <- forAll $ G.int (R.linear 0 100)
  True === True
  a === a
  H.success

test_spec :: H.TestT IO ()
test_spec = unit do
  True === True
  True === True
  H.success
