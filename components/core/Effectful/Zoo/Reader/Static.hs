module Effectful.Zoo.Reader.Static
  ( Reader,
    ask,
    asks,
    local,
    runReader,

    runReaderM,
  ) where

import Effectful
import Effectful.Reader.Static
import HaskellWorks.Prelude

runReaderM ::
    forall i r a.
    Eff r i ->
    Eff (Reader i ': r) a ->
    Eff r a
runReaderM f action = do
  f >>= flip runReader action
