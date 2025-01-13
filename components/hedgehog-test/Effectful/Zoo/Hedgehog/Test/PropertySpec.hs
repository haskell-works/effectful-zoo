{- HLINT ignore "Use camelCase" -}

module Effectful.Zoo.Hedgehog.Test.PropertySpec where

import Effectful.Zoo.Hedgehog
import Effectful.Zoo.Hedgehog.Effect.Run
import HaskellWorks.Prelude
import Hedgehog qualified as H
import Hedgehog.Gen qualified as G
import Hedgehog.Range qualified as R

tasty_property_spec :: PropertyT IO ()
tasty_property_spec = property do
  a <- forAll $ G.int (R.linear 0 100)
  True === True
  a === a
  H.success

tasty_unit_spec :: TestT IO ()
tasty_unit_spec = unit do
  True === True
  True === True
  H.success
