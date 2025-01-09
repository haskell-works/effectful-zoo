module Effectful.Zoo.Hedgehog.Api.Assert
  ( onNothingFail,
    onNothingFailM,
    onLeftFail,
    onLeftFailM,

    trapFail,
    trapFailJson,
    trapFailJsonPretty,
    trapFailYaml,

    failMessage,
    failWithCustom,

    byDurationM,
    byDeadlineM,
  ) where

import Data.Aeson
import Data.Aeson qualified as J
import Data.Aeson.Encode.Pretty qualified as J
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as LT
import Data.Time.Clock (NominalDiffTime, UTCTime)
import Data.Time.Clock qualified as DTC
import Data.Yaml qualified as Y
import Effectful
import Effectful.Concurrent
import Effectful.Dispatch.Dynamic
import Effectful.Zoo.Core
import Effectful.Zoo.Error.Static
import Effectful.Zoo.Hedgehog.Api.Hedgehog
import Effectful.Zoo.Hedgehog.Api.Journal
import Effectful.Zoo.Hedgehog.Api.MonadAssertion
import Effectful.Zoo.Hedgehog.Effect.Hedgehog
import GHC.Stack qualified as GHC
import HaskellWorks.Prelude
import Hedgehog (MonadTest(..))
import Hedgehog qualified as H
import Hedgehog.Internal.Property qualified as H
import Hedgehog.Internal.Source qualified as H

onNothingFail :: forall a m. ()
  => HasCallStack
  => MonadTest m
  => Maybe a
  -> m a
onNothingFail mv =
  withFrozenCallStack $
    case mv of
      Just a -> pure a
      Nothing -> failWith Nothing "Expected Just, but got Nothing"

onNothingFailM :: forall a m. ()
  => HasCallStack
  => MonadTest m
  => m (Maybe a)
  -> m a
onNothingFailM f =
  withFrozenCallStack $
    f >>= onNothingFail

onLeftFail :: forall e a m. ()
  => HasCallStack
  => MonadTest m
  => Show e
  => Either e a
  -> m a
onLeftFail ea =
  withFrozenCallStack $
    case ea of
      Right a -> pure a
      Left e -> failWith Nothing $ "Expected Just, but got Left: " <> show e

onLeftFailM :: forall e a m. ()
  => HasCallStack
  => MonadTest m
  => Show e
  => m (Either e a)
  -> m a
onLeftFailM f =
  withFrozenCallStack $
    f >>= onLeftFail

failMessage :: ()
  => H.MonadTest m
  => CallStack
  -> String
  -> m a
failMessage cs =
  withFrozenCallStack $
    failWithCustom cs Nothing

failWithCustom :: ()
  => H.MonadTest m
  => CallStack
  -> Maybe H.Diff
  -> String
  -> m a
failWithCustom cs mdiff msg =
  H.liftTest $ H.mkTest (Left $ H.Failure (H.getCaller cs) msg mdiff, mempty)

trapFail :: forall e a r. ()
  => HasCallStack
  => r <: Concurrent
  => r <: Error H.Failure
  => r <: Hedgehog
  => Show e
  => Eff (Error e ': r) a
  -> Eff r a
trapFail f =
  withFrozenCallStack do
    r <- f & runError_
    case r of
      Right a -> pure a
      Left e  -> failWith Nothing $ show e

trapFailJson :: forall e a r. ()
  => HasCallStack
  => r <: Concurrent
  => r <: Error H.Failure
  => r <: Hedgehog
  => ToJSON e
  => Eff (Error e ': r) a
  -> Eff r a
trapFailJson f =
  withFrozenCallStack do
    r <- withFrozenCallStack $ f & runError_
    case r of
      Right a -> pure a
      Left e  -> do
        let msg = LT.unpack $ LT.decodeUtf8 $ J.encode e
        failWith Nothing msg

trapFailJsonPretty :: forall e a r. ()
  => HasCallStack
  => r <: Concurrent
  => r <: Error H.Failure
  => r <: Hedgehog
  => ToJSON e
  => Eff (Error e ': r) a
  -> Eff r a
trapFailJsonPretty f =
  withFrozenCallStack do
    r <- withFrozenCallStack $ f & runError_
    case r of
      Right a -> pure a
      Left e  -> do
        let msg = LT.unpack $ LT.decodeUtf8 $ J.encodePretty e
        failWith Nothing msg

trapFailYaml :: forall e a r. ()
  => HasCallStack
  => r <: Concurrent
  => r <: Error H.Failure
  => r <: Hedgehog
  => ToJSON e
  => Eff (Error e ': r) a
  -> Eff r a
trapFailYaml f =
  withFrozenCallStack do
    r <- withFrozenCallStack $ f & runError_
    case r of
      Right a -> pure a
      Left e  -> do
        let msg = T.unpack $ T.decodeUtf8 $ Y.encode e
        failWith Nothing msg

-- | Run the operation 'f' once a second until it returns 'True' or the deadline expires.
--
-- Expiration of the deadline results in an assertion failure
byDeadlineM :: forall a r. ()
  => HasCallStack
  => r <: Concurrent
  => r <: Error H.Failure
  => r <: Hedgehog
  => r <: IOE
  => NominalDiffTime
  -> UTCTime
  -> String
  -> Eff r a
  -> Eff r a
byDeadlineM period deadline errorMessage f = GHC.withFrozenCallStack $ do
  start <- liftIO DTC.getCurrentTime
  a <- goM
  end <- liftIO DTC.getCurrentTime
  jot_ $ "Operation completed in " <> tshow (DTC.diffUTCTime end start)
  return a
  where goM = catchAssertion f $ \e -> do
          currentTime <- liftIO DTC.getCurrentTime
          if currentTime < deadline
            then do
              threadDelay (floor (DTC.nominalDiffTimeToSeconds period * 1000000))
              goM
            else do
              jotShow_ currentTime
              void $ failMessage GHC.callStack $ "Condition not met by deadline: " <> errorMessage
              throwAssertion e

-- | Run the operation 'f' once a second until it returns 'True' or the duration expires.
--
-- Expiration of the duration results in an assertion failure
byDurationM :: forall b r. ()
  => HasCallStack
  => r <: Concurrent
  => r <: Error H.Failure
  => r <: IOE
  => r <: Hedgehog
  => NominalDiffTime
  -> NominalDiffTime
  -> String
  -> Eff r b
  -> Eff r b
byDurationM period duration errorMessage f = GHC.withFrozenCallStack $ do
  deadline <- DTC.addUTCTime duration <$> liftIO DTC.getCurrentTime
  byDeadlineM period deadline errorMessage f
