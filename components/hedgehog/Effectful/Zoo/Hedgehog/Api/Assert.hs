module Effectful.Zoo.Hedgehog.Api.Assert
  ( assert,
    (===),
    (/==),
    onNothingFail,
    onNothingFailM,
    onLeftFail,
    onLeftFailM,

    trapFail,
    trapFailJson,
    trapFailJsonPretty,
    trapFailYaml,
  ) where

import Data.Aeson
import Data.Aeson qualified as J
import Data.Aeson.Encode.Pretty qualified as J
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as LT
import Data.Yaml qualified as Y
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Zoo.Core
import Effectful.Zoo.Core.Error.Static
import Effectful.Zoo.Hedgehog.Api.Failure
import Effectful.Zoo.Hedgehog.Dynamic
import HaskellWorks.Prelude
import Hedgehog qualified as H

infix 4 ===, /==

assert :: forall r. ()
  => HasCallStack
  => r <: Hedgehog
  => Bool
  -> Eff r ()
assert condition =
  withFrozenCallStack $
    H.assert condition

(===) :: forall a r. ()
  => HasCallStack
  => Eq a
  => Show a
  => r <: Hedgehog
  => a
  -> a
  -> Eff r ()
(===) a b =
  withFrozenCallStack $
    a H.=== b

(/==) :: forall a r. ()
  => HasCallStack
  => Eq a
  => Show a
  => r <: Hedgehog
  => a
  -> a
  -> Eff r ()
(/==) a b =
  withFrozenCallStack $
    a H./== b

onNothingFail :: forall a r. ()
  => HasCallStack
  => r <: Hedgehog
  => Maybe a
  -> Eff r a
onNothingFail mv =
  withFrozenCallStack $
    case mv of
      Just a -> pure a
      Nothing -> failWith Nothing "Expected Just, but got Nothing"

onNothingFailM :: forall a r. ()
  => HasCallStack
  => r <: Hedgehog
  => Eff r (Maybe a)
  -> Eff r a
onNothingFailM f =
  withFrozenCallStack $
    f >>= onNothingFail

onLeftFail :: forall e a r. ()
  => HasCallStack
  => Show e
  => r <: Hedgehog
  => Either e a
  -> Eff r a
onLeftFail ea =
  withFrozenCallStack $
    case ea of
      Right a -> pure a
      Left e -> failWith Nothing $ "Expected Just, but got Left: " <> show e

onLeftFailM :: forall e a r. ()
  => HasCallStack
  => Show e
  => r <: Hedgehog
  => Eff r (Either e a)
  -> Eff r a
onLeftFailM f =
  withFrozenCallStack $
    f >>= onLeftFail

trapFail :: forall e a r. ()
  => HasCallStack
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
