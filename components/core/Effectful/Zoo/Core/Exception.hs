module Effectful.Zoo.Core.Exception
  ( Exception,
    catchIO,
    throwIO,
    trapIO,
  ) where

import Effectful
import Effectful.Exception (Exception)
import Effectful.Exception qualified as E
import HaskellWorks.Prelude

catchIO :: ()
  => E.Exception e
  => Eff es a
  -> (e -> Eff es a)
  -> Eff es a
catchIO =
  E.catch

throwIO :: ()
  => HasCallStack
  => E.Exception e
  => e
  -> Eff es a
throwIO =
  E.throwIO

trapIO
  :: E.Exception e
  => (e -> Eff es a)
  -> Eff es a
  -> Eff es a
trapIO =
  flip catchIO
