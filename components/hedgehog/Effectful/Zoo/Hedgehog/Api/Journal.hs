module Effectful.Zoo.Hedgehog.Api.Journal
  ( jot,
    jot_,
    
    jotWithCallStack,

    jotText_,
    jotM,
    jotBsUtf8M,
    jotLbsUtf8M,
    jotM_,
    jotIO,
    jotIO_,
    jotShow,
    jotShow_,
    jotShowM,
    jotShowM_,
    jotShowIO,
    jotShowIO_,
    jotShowRead,
    jotJson,
    jotJson_,
    jotJsonM,
    jotJsonM_,
    jotJsonPretty,
    jotJsonPretty_,
    jotJsonPrettyM,
    jotJsonPrettyM_,
    jotYaml,
    jotYaml_,
    jotYamlM,
    jotYamlM_,
    jotEach,
    jotEach_,
    jotEachM,
    jotEachM_,
    jotEachIO,
    jotEachIO_,

    jotLogTextWithCallStack,

    jotShowDataLog,

    writeLog,
  ) where

import Data.Aeson (ToJSON(..))
import Data.Aeson qualified as J
import Data.Aeson.Encode.Pretty qualified as J
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as LT
import Data.Traversable
import Data.Yaml qualified as Y
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Zoo.Core
import Effectful.Zoo.DataLog.Dynamic
import Effectful.Zoo.DataLog.Dynamic qualified as DataLog
import Effectful.Zoo.Hedgehog.Api.Eval
import Effectful.Zoo.Hedgehog.Api.Failure
import Effectful.Zoo.Hedgehog.Dynamic
import Effectful.Zoo.Log.Data.Severity
import GHC.Stack qualified as GHC
import HaskellWorks.Prelude
import HaskellWorks.String
import HaskellWorks.ToText
import Hedgehog.Internal.Property qualified as H
import Hedgehog.Internal.Source qualified as H

-- | Annotate the given string at the context supplied by the callstack.
jotWithCallStack :: forall r. ()
  => r <: Hedgehog
  => CallStack
  -> String
  -> Eff r ()
jotWithCallStack cs a =
  writeLog $ H.Annotation (H.getCaller cs) a

-- | Annotate with the given string.
jot :: forall r. ()
  => r <: Hedgehog
  => HasCallStack
  => String
  -> Eff r String
jot a =
  withFrozenCallStack do
    !b <- eval a
    jotWithCallStack GHC.callStack b
    return b

-- | Annotate the given string returning unit.
jot_ :: forall r. ()
  => r <: Hedgehog
  => HasCallStack
  => Text
  -> Eff r ()
jot_ =
  withFrozenCallStack do
    jotText_

-- | Annotate the given text returning unit.
jotText_ :: forall r. ()
  => r <: Hedgehog
  => HasCallStack
  => Text
  -> Eff r ()
jotText_ a =
  withFrozenCallStack do
    jotWithCallStack GHC.callStack $ T.unpack a

-- | Annotate the given string in a monadic context.
jotM :: forall a r. ()
  => ToString a
  => r <: Hedgehog
  => HasCallStack
  => Eff r a
  -> Eff r a
jotM a =
  withFrozenCallStack do
    !b <- evalM a
    jotWithCallStack GHC.callStack $ toString b
    return b

jotBsUtf8M :: forall r. ()
  => r <: Hedgehog
  => HasCallStack
  => Eff r ByteString
  -> Eff r ByteString
jotBsUtf8M a =
  withFrozenCallStack do
    !b <- evalM a
    jotWithCallStack GHC.callStack $ T.unpack $ T.decodeUtf8 b
    return b

jotLbsUtf8M :: forall r. ()
  => r <: Hedgehog
  => HasCallStack
  => Eff r LBS.ByteString
  -> Eff r LBS.ByteString
jotLbsUtf8M a =
  withFrozenCallStack do
    !b <- evalM a
    jotWithCallStack GHC.callStack $ LT.unpack $ LT.decodeUtf8 b
    return b

-- | Annotate the given string in a monadic context returning unit.
jotM_ :: forall r. ()
  => r <: Hedgehog
  => HasCallStack
  => Eff r String
  -> Eff r ()
jotM_ a =
  withFrozenCallStack do
    !b <- evalM a
    jotWithCallStack GHC.callStack b
    return ()

-- | Annotate the given string in IO.
jotIO :: forall r. ()
  => r <: Hedgehog
  => r <: IOE
  => HasCallStack
  => IO String
  -> Eff r String
jotIO f =
  withFrozenCallStack do
    !a <- evalIO f
    jotWithCallStack GHC.callStack a
    return a

-- | Annotate the given string in IO returning unit.
jotIO_ :: forall r. ()
  => r <: Hedgehog
  => r <: IOE
  => HasCallStack
  => IO String
  -> Eff r ()
jotIO_ f =
  withFrozenCallStack do
    !a <- evalIO f
    jotWithCallStack GHC.callStack a
    return ()

-- | Annotate the given value.
jotShow :: forall a r. ()
  => r <: Hedgehog
  => HasCallStack
  => Show a
  => a
  -> Eff r a
jotShow a =
  withFrozenCallStack do
    !b <- eval a
    jotWithCallStack GHC.callStack (show b)
    return b

-- | Annotate the given value returning unit.
jotShow_ :: forall a r. ()
  => r <: Hedgehog
  => HasCallStack
  => Show a
  => a
  -> Eff r ()
jotShow_ a =
  withFrozenCallStack do
    jotWithCallStack GHC.callStack (show a)

-- | Annotate the given value in a monadic context.
jotShowM :: forall a r. ()
  => r <: Hedgehog
  => HasCallStack
  => Show a
  => Eff r a
  -> Eff r a
jotShowM a =
  withFrozenCallStack do
    !b <- evalM a
    jotWithCallStack GHC.callStack (show b)
    return b

-- | Annotate the given value in a monadic context returning unit.
jotShowM_ :: forall a r. ()
  => r <: Hedgehog
  => HasCallStack
  => Show a
  => Eff r a
  -> Eff r ()
jotShowM_ a =
  withFrozenCallStack do
    !b <- evalM a
    jotWithCallStack GHC.callStack (show b)
    return ()

-- | Annotate the given value in IO.
jotShowIO :: forall a r. ()
  => r <: Hedgehog
  => r <: IOE
  => HasCallStack
  => Show a
  => IO a
  -> Eff r a
jotShowIO f =
  withFrozenCallStack do
    !a <- evalIO f
    jotWithCallStack GHC.callStack (show a)
    return a

-- | Annotate the given value in IO returning unit.
jotShowIO_ :: forall a r. ()
  => r <: Hedgehog
  => r <: IOE
  => HasCallStack
  => Show a
  => IO a
  -> Eff r ()
jotShowIO_ f =
  withFrozenCallStack do
    !a <- evalIO f
    jotWithCallStack GHC.callStack (show a)
    return ()

-- | Annotate the given value.
jotShowRead :: forall a r. ()
  => HasCallStack
  => r <: Hedgehog
  => Read a
  => Show a
  => String
  -> Eff r a
jotShowRead s =
  withFrozenCallStack do
    !result <- eval (readEither @a s)
    case result of
      Left e -> failWith Nothing $ "Failed to parse: " <> show s <> " with error: " <> show e
      Right a -> do
        jotWithCallStack GHC.callStack (show a)
        return a

-- | Annotate the given value as JSON.
jotJson :: forall a r. ()
  => r <: Hedgehog
  => HasCallStack
  => ToJSON a
  => a
  -> Eff r a
jotJson a =
  withFrozenCallStack do
    !b <- eval a
    jotWithCallStack GHC.callStack $ LT.unpack $ LT.decodeUtf8 $ J.encode b
    return b

-- | Annotate the given value as JSON.
jotJson_ :: forall a r. ()
  => r <: Hedgehog
  => HasCallStack
  => ToJSON a
  => a
  -> Eff r ()
jotJson_ a =
  withFrozenCallStack do
    !b <- eval a
    jotWithCallStack GHC.callStack $ LT.unpack $ LT.decodeUtf8 $ J.encode b
    return ()

-- | Annotate the given value as JSON in a monadic context.
jotJsonM :: forall a r. ()
  => r <: Hedgehog
  => HasCallStack
  => ToJSON a
  => Eff r a
  -> Eff r a
jotJsonM a =
  withFrozenCallStack do
    !b <- evalM a
    jotWithCallStack GHC.callStack $ LT.unpack $ LT.decodeUtf8 $ J.encode b
    return b

-- | Annotate the given value as JSON in a monadic context.
jotJsonM_ :: forall a r. ()
  => r <: Hedgehog
  => HasCallStack
  => ToJSON a
  => Eff r a
  -> Eff r ()
jotJsonM_ a =
  withFrozenCallStack do
    !b <- evalM a
    jotWithCallStack GHC.callStack $ LT.unpack $ LT.decodeUtf8 $ J.encode b
    return ()

-- | Annotate the given value as JSON.
jotJsonPretty :: forall a r. ()
  => r <: Hedgehog
  => HasCallStack
  => ToJSON a
  => a
  -> Eff r a
jotJsonPretty a =
  withFrozenCallStack do
    !b <- eval a
    jotWithCallStack GHC.callStack $ LT.unpack $ LT.decodeUtf8 $ J.encodePretty b
    return b

-- | Annotate the given value as JSON.
jotJsonPretty_ :: forall a r. ()
  => r <: Hedgehog
  => HasCallStack
  => ToJSON a
  => a
  -> Eff r ()
jotJsonPretty_ a =
  withFrozenCallStack do
    !b <- eval a
    jotWithCallStack GHC.callStack $ LT.unpack $ LT.decodeUtf8 $ J.encodePretty b
    return ()

-- | Annotate the given value as JSON in a monadic context.
jotJsonPrettyM :: forall a r. ()
  => r <: Hedgehog
  => HasCallStack
  => ToJSON a
  => Eff r a
  -> Eff r a
jotJsonPrettyM a =
  withFrozenCallStack do
    !b <- evalM a
    jotWithCallStack GHC.callStack $ LT.unpack $ LT.decodeUtf8 $ J.encodePretty b
    return b

-- | Annotate the given value as JSON in a monadic context.
jotJsonPrettyM_ :: forall a r. ()
  => r <: Hedgehog
  => HasCallStack
  => ToJSON a
  => Eff r a
  -> Eff r ()
jotJsonPrettyM_ a =
  withFrozenCallStack do
    !b <- evalM a
    jotWithCallStack GHC.callStack $ LT.unpack $ LT.decodeUtf8 $ J.encodePretty b
    return ()

-- | Annotate the given value as JSON.
jotYaml :: forall a r. ()
  => r <: Hedgehog
  => HasCallStack
  => ToJSON a
  => a
  -> Eff r a
jotYaml a =
  withFrozenCallStack do
    !b <- eval a
    jotWithCallStack GHC.callStack $ T.unpack $ T.decodeUtf8 $ Y.encode b
    return b

-- | Annotate the given value as JSON.
jotYaml_ :: forall a r. ()
  => r <: Hedgehog
  => HasCallStack
  => ToJSON a
  => a
  -> Eff r ()
jotYaml_ a =
  withFrozenCallStack do
    !b <- eval a
    jotWithCallStack GHC.callStack $ T.unpack $ T.decodeUtf8 $ Y.encode b
    return ()

-- | Annotate the given value as JSON in a monadic context.
jotYamlM :: forall a r. ()
  => r <: Hedgehog
  => HasCallStack
  => ToJSON a
  => Eff r a
  -> Eff r a
jotYamlM a =
  withFrozenCallStack do
    !b <- evalM a
    jotWithCallStack GHC.callStack $ T.unpack $ T.decodeUtf8 $ Y.encode b
    return b

-- | Annotate the given value as JSON in a monadic context.
jotYamlM_ :: forall a r. ()
  => r <: Hedgehog
  => HasCallStack
  => ToJSON a
  => Eff r a
  -> Eff r ()
jotYamlM_ a =
  withFrozenCallStack do
    !b <- evalM a
    jotWithCallStack GHC.callStack $ T.unpack $ T.decodeUtf8 $ Y.encode b
    return ()

-- | Annotate the each value in the given traversable.
jotEach :: forall a f r. ()
  => r <: Hedgehog
  => HasCallStack
  => Show a
  => Traversable f
  => f a
  -> Eff r (f a)
jotEach as =
  withFrozenCallStack do
    for_ as $ jotWithCallStack GHC.callStack . show
    return as

-- | Annotate the each value in the given traversable returning unit.
jotEach_ :: forall a f r. ()
  => r <: Hedgehog
  => HasCallStack
  => Show a
  => Traversable f
  => f a
  -> Eff r ()
jotEach_ as =
  withFrozenCallStack $ for_ as $ jotWithCallStack GHC.callStack . show

-- | Annotate the each value in the given traversable in a monadic context.
jotEachM :: forall a f r. ()
  => r <: Hedgehog
  => HasCallStack
  => Show a
  => Traversable f
  => Eff r (f a)
  -> Eff r (f a)
jotEachM f =
  withFrozenCallStack do
    !as <- f
    for_ as $ jotWithCallStack GHC.callStack . show
    return as

-- | Annotate the each value in the given traversable in a monadic context returning unit.
jotEachM_ :: forall a f r. ()
  => r <: Hedgehog
  => HasCallStack
  => Show a
  => Traversable f
  => Eff r (f a)
  -> Eff r ()
jotEachM_ f =
  withFrozenCallStack do
    !as <- f
    for_ as $ jotWithCallStack GHC.callStack . show

-- | Annotate the each value in the given traversable in IO.
jotEachIO :: forall a f r. ()
  => r <: Hedgehog
  => r <: IOE
  => HasCallStack
  => Show a
  => Traversable f
  => IO (f a)
  -> Eff r (f a)
jotEachIO f =
  withFrozenCallStack do
    !as <- evalIO f
    for_ as $ jotWithCallStack GHC.callStack . show
    return as

-- | Annotate the each value in the given traversable in IO returning unit.
jotEachIO_ :: forall a f r. ()
  => r <: Hedgehog
  => r <: IOE
  => HasCallStack
  => Show a
  => Traversable f
  => IO (f a)
  -> Eff r ()
jotEachIO_ f =
  withFrozenCallStack do
    !as <- evalIO f
    for_ as $ jotWithCallStack GHC.callStack . show

jotLogTextWithCallStack :: forall r. ()
  => r <: Hedgehog
  => CallStack
  -> Severity
  -> Text
  -> Eff r ()
jotLogTextWithCallStack cs severity a =
  withFrozenCallStack do
    jotWithCallStack cs $ T.unpack $ "[" <> toText severity <> "] " <> a

jotShowDataLog :: forall i a r. ()
  => HasCallStack
  => Show i
  => r <: Hedgehog
  => Eff (DataLog i : r) a
  -> Eff r a
jotShowDataLog =
  withFrozenCallStack $
    DataLog.runDataLog jotShow_
{-# inline jotShowDataLog #-}

writeLog :: forall r. ()
  => HasCallStack
  => r <: Hedgehog
  => H.Log
  -> Eff r ()
writeLog message =
  withFrozenCallStack $
    H.writeLog message
