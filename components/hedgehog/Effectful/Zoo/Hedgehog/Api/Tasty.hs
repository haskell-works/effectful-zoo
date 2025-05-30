module Effectful.Zoo.Hedgehog.Api.Tasty
  ( ToTestTree(..),
  ) where

import HaskellWorks.Prelude
import Hedgehog (PropertyT, TestT)
import Hedgehog qualified as H
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Hedgehog (testProperty)

class ToTestTree a where
  toTestTree :: TestName -> a -> TestTree

instance ToTestTree (PropertyT IO ()) where
  toTestTree desc =
    testProperty desc . H.property

instance ToTestTree (TestT IO ()) where
  toTestTree desc =
    testProperty desc . H.withTests 1 . H.property . H.test
