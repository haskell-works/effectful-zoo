module Effectful.Zoo.Hedgehog.MonadTestProxy
  ( MonadTestProxy(..),
  ) where

import Control.Monad.Trans.Writer.Lazy qualified as MTL
import Data.Functor.Identity
import Effectful
import Effectful.Error.Static
import Effectful.Writer.Static.Local
import Effectful.Zoo.Core
import HaskellWorks.Prelude
import Hedgehog (MonadTest(..))
import Hedgehog.Internal.Property (Failure, Journal)
import Hedgehog.Internal.Property qualified as H

class Monad m => MonadTestProxy m where
  liftTestProxy :: H.Test a -> m a

instance MonadTestProxy (H.TestT IO) where
  liftTestProxy = liftTest

instance (r <: Error Failure, r <: Writer Journal) => MonadTestProxy (Eff r) where
  liftTestProxy = \case
    H.TestT m -> do
      let (result, journal) = runIdentity $ MTL.runWriterT $ runExceptT m
      tell journal
      case result of
        Left (H.Failure loc err diff) ->
          throwError (H.Failure loc err diff)
        Right a ->
          pure a
