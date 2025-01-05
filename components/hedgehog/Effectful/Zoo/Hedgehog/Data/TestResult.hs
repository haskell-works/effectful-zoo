module Effectful.Zoo.Hedgehog.Data.TestResult
  ( TestResult(..),
  ) where

import HaskellWorks.Prelude
import Hedgehog.Internal.Property qualified as H

data TestResult a
  = TestResult a
  | TestFailure H.Failure
  | TestError SomeException
