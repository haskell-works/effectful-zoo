module Effectful.Zoo.Hedgehog.Test.HedgehogTest where

import Effectful
import Effectful.Zoo.Core
import Effectful.Zoo.Log.Dynamic
import Effectful.Zoo.Log.Api.Text
import Effectful.Zoo.Hedgehog
import HaskellWorks.Prelude

foo :: ()
  => HasCallStack
  => r <: Log Text
  => Eff r ()
foo =
  info "This is a log"

test_simple :: UnitTest
test_simple =
  hedgehog do
    jot_ "This is a jot"

    foo
