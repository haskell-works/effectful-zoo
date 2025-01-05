module Effectful.Zoo.Hedgehog.Api.MonadAssertion
  ( MonadAssertion(..),
    tryAssertion,
    tryExceptAssertion,
  ) where

import Control.Monad.Catch (MonadCatch(..))
import Control.Monad.Catch qualified as C
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except qualified as E
import Control.Monad.Trans.Resource qualified as IO
import Control.Monad.Trans.Resource.Internal qualified as IO
import Effectful.Zoo.Hedgehog.Data.TestResult
import HaskellWorks.Prelude
import Hedgehog.Internal.Property qualified as H

class Monad m => MonadAssertion m where
  throwAssertion :: H.Failure -> m a
  catchAssertion :: m a -> (H.Failure -> m a) -> m a

instance Monad m => MonadAssertion (H.TestT m) where
  throwAssertion f = H.liftTest $ H.mkTest (Left f, mempty)
  catchAssertion g h = H.TestT $ E.catchE (H.unTest g) (H.unTest . h)

instance MonadAssertion m => MonadAssertion (IO.ResourceT m) where
  throwAssertion = lift . throwAssertion
  catchAssertion r h = IO.ResourceT $ \i -> IO.unResourceT r i `catchAssertion` \e -> IO.unResourceT (h e) i

deriving newtype instance Monad m => MonadAssertion (H.PropertyT m)

tryAssertion :: ()
  => MonadAssertion m
  => m a
  -> m (Either H.Failure a)
tryAssertion m =
  catchAssertion (Right <$> m) (pure . Left)

tryExceptAssertion :: ()
  => MonadAssertion m
  => MonadCatch m
  => m a
  -> m (TestResult a)
tryExceptAssertion m =
  tryAssertion (C.try m) >>= \case
    Right (Right a) -> pure $ TestResult a
    Right (Left e) -> pure $ TestError e
    Left f -> pure $ TestFailure f
