{-# OPTIONS_GHC -fno-warn-orphans #-}

module Effectful.Zoo.Hedgehog.Api.Tasty.Orphans where

import Data.Monoid
import HaskellWorks.Prelude
import Hedgehog (PropertyT, TestT)
import Hedgehog qualified as H
import Test.Tasty.Discover
import Test.Tasty.Discover.TastyInfo
import Test.Tasty.Hedgehog

instance Tasty (PropertyT IO ()) where
  tasty info = pure . testProperty testName . H.property
    where testName = fromMaybe "" $ getLast info.name

instance Tasty (TestT IO ()) where
  tasty info = tasty info . H.test
