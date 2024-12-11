module Effectful.Zoo.HUnit
  ( requireTest,
  ) where

import Control.Monad.IO.Class
import Data.List qualified as L
import GHC.Stack (SrcLoc)
import GHC.Stack qualified as GHC
import HaskellWorks.Prelude
import Hedgehog
import Test.HUnit.Lang

import qualified Control.Exception as E

location :: HasCallStack => Maybe SrcLoc
location = case L.reverse (GHC.getCallStack GHC.callStack) of
  (_, loc) : _ -> Just loc
  []           -> Nothing

require :: HasCallStack => Property -> Assertion
require p = do
  result <- liftIO $ check p
  unless result $
    E.throwIO (HUnitFailure location $ Reason "Hedgehog property test failed")

requireTest :: HasCallStack => TestT IO () -> Assertion
requireTest = require . withTests 1 . property . test
