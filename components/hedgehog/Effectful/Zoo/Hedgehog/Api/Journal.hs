module Effectful.Zoo.Hedgehog.Api.Journal
  ( Failure,

    jot,
    jot_,
    
    jotWithCallStack,
    jotTextWithCallStack,

    jotString,
    jotString_,
    jotText,
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

    jotPkgInputFile,
    jotPkgGoldenFile,
    jotRootInputFile,
    jotTempFile,

    jotLogTextWithCallStack,

    jotShowDataLog,

    writeLog,
  ) where

import Control.Monad.Catch (MonadCatch)
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
import Effectful.Concurrent
import Effectful.Dispatch.Dynamic
import Effectful.Zoo.Core
import Effectful.Zoo.DataLog.Dynamic
import Effectful.Zoo.DataLog.Dynamic qualified as DataLog
import Effectful.Zoo.Error.Static
import Effectful.Zoo.Hedgehog.Api.Hedgehog
import Effectful.Zoo.Hedgehog.Data.PackagePath
import Effectful.Zoo.Hedgehog.Data.ProjectRoot
import Effectful.Zoo.Hedgehog.Data.Workspace
import Effectful.Zoo.Hedgehog.Effect.Hedgehog
import Effectful.Zoo.Log.Data.Severity
import Effectful.Zoo.Reader.Static
import GHC.Stack qualified as GHC
import HaskellWorks.Prelude
import HaskellWorks.String
import HaskellWorks.ToText
import Hedgehog (MonadTest(..))
import Hedgehog.Internal.Property (Failure)
import Hedgehog.Internal.Property qualified as H
import Hedgehog.Internal.Source qualified as H

-- | Annotate the given string at the context supplied by the callstack.
jotWithCallStack :: forall m. ()
  => MonadTest m
  => CallStack
  -> String
  -> m ()
jotWithCallStack cs a =
  writeLog $ H.Annotation (H.getCaller cs) a

-- | Annotate the given string at the context supplied by the callstack.
jotTextWithCallStack :: forall m. ()
  => MonadTest m
  => CallStack
  -> Text
  -> m ()
jotTextWithCallStack cs a =
  writeLog $ H.Annotation (H.getCaller cs) $ T.unpack a

-- | Annotate with the given string.
jot :: forall m. ()
  => HasCallStack
  => MonadTest m
  => Text
  -> m Text
jot a =
  withFrozenCallStack do
    jotText a

-- | Annotate the given string returning unit.
jot_ :: forall m. ()
  => HasCallStack
  => MonadTest m
  => Text
  -> m ()
jot_ a =
  withFrozenCallStack do
    jotText_ a

-- | Annotate with the given string.
jotString :: forall m. ()
  => HasCallStack
  => MonadTest m
  => String
  -> m String
jotString a =
  withFrozenCallStack do
    !b <- eval a
    jotWithCallStack GHC.callStack b
    pure b

-- | Annotate with the given string.
jotString_ :: forall m. ()
  => HasCallStack
  => MonadTest m
  => String
  -> m ()
jotString_ a =
  withFrozenCallStack do
    !b <- eval a
    jotWithCallStack GHC.callStack b

-- | Annotate the given text returning unit.
jotText :: forall m. ()
  => HasCallStack
  => MonadTest m
  => Text
  -> m Text
jotText a =
  withFrozenCallStack do
    !b <- eval a
    jotWithCallStack GHC.callStack $ T.unpack a
    pure b

-- | Annotate the given text returning unit.
jotText_ :: forall m. ()
  => HasCallStack
  => MonadTest m
  => Text
  -> m ()
jotText_ a =
  withFrozenCallStack do
    !b <- eval a
    jotWithCallStack GHC.callStack $ T.unpack b

-- | Annotate the given string in a monadic context.
jotM :: forall a m. ()
  => HasCallStack
  => MonadCatch m
  => MonadTest m
  => ToString a
  => m a
  -> m a
jotM a =
  withFrozenCallStack do
    !b <- evalM a
    jotWithCallStack GHC.callStack $ toString b
    return b

jotBsUtf8M :: forall m. ()
  => HasCallStack
  => MonadCatch m
  => MonadTest m
  => m ByteString
  -> m ByteString
jotBsUtf8M a =
  withFrozenCallStack do
    !b <- evalM a
    jotWithCallStack GHC.callStack $ T.unpack $ T.decodeUtf8 b
    return b

jotLbsUtf8M :: forall m. ()
  => HasCallStack
  => MonadCatch m
  => MonadTest m
  => m LBS.ByteString
  -> m LBS.ByteString
jotLbsUtf8M a =
  withFrozenCallStack do
    !b <- evalM a
    jotWithCallStack GHC.callStack $ LT.unpack $ LT.decodeUtf8 b
    return b

-- | Annotate the given string in a monadic context returning unit.
jotM_ :: forall m. ()
  => HasCallStack
  => MonadCatch m
  => MonadTest m
  => m String
  -> m ()
jotM_ a =
  withFrozenCallStack do
    !b <- evalM a
    jotWithCallStack GHC.callStack b
    return ()

-- | Annotate the given string in IO.
jotIO :: forall m. ()
  => HasCallStack
  => MonadIO m
  => MonadTest m
  => IO String
  -> m String
jotIO f =
  withFrozenCallStack do
    !a <- evalIO f
    jotWithCallStack GHC.callStack a
    return a

-- | Annotate the given string in IO returning unit.
jotIO_ :: forall m. ()
  => HasCallStack
  => MonadIO m
  => MonadTest m
  => IO String
  -> m ()
jotIO_ f =
  withFrozenCallStack do
    !a <- evalIO f
    jotWithCallStack GHC.callStack a
    return ()

-- | Annotate the given value.
jotShow :: forall a m. ()
  => HasCallStack
  => MonadIO m
  => MonadTest m
  => Show a
  => a
  -> m a
jotShow a =
  withFrozenCallStack do
    !b <- eval a
    jotWithCallStack GHC.callStack (show b)
    return b

-- | Annotate the given value returning unit.
jotShow_ :: forall a m. ()
  => HasCallStack
  => MonadTest m
  => Show a
  => a
  -> m ()
jotShow_ a =
  withFrozenCallStack do
    jotWithCallStack GHC.callStack (show a)

-- | Annotate the given value in a monadic context.
jotShowM :: forall a m. ()
  => HasCallStack
  => MonadCatch m
  => MonadTest m
  => Show a
  => m a
  -> m a
jotShowM a =
  withFrozenCallStack do
    !b <- evalM a
    jotWithCallStack GHC.callStack (show b)
    return b

-- | Annotate the given value in a monadic context returning unit.
jotShowM_ :: forall a m. ()
  => HasCallStack
  => MonadCatch m
  => MonadTest m
  => Show a
  => m a
  -> m ()
jotShowM_ a =
  withFrozenCallStack do
    !b <- evalM a
    jotWithCallStack GHC.callStack (show b)
    return ()

-- | Annotate the given value in IO.
jotShowIO :: forall a m. ()
  => HasCallStack
  => MonadIO m
  => MonadTest m
  => Show a
  => IO a
  -> m a
jotShowIO f =
  withFrozenCallStack do
    !a <- evalIO f
    jotWithCallStack GHC.callStack (show a)
    return a

-- | Annotate the given value in IO returning unit.
jotShowIO_ :: forall a m. ()
  => HasCallStack
  => MonadIO m
  => MonadTest m
  => Show a
  => IO a
  -> m ()
jotShowIO_ f =
  withFrozenCallStack do
    !a <- evalIO f
    jotWithCallStack GHC.callStack (show a)
    return ()

-- | Annotate the given value.
jotShowRead :: forall a m. ()
  => HasCallStack
  => MonadIO m
  => MonadTest m
  => Read a
  => Show a
  => String
  -> m a
jotShowRead s =
  withFrozenCallStack do
    !result <- eval (readEither @a s)
    case result of
      Left e -> failWith Nothing $ "Failed to parse: " <> show s <> " with error: " <> show e
      Right a -> do
        jotWithCallStack GHC.callStack (show a)
        return a

-- | Annotate the given value as JSON.
jotJson :: forall a m. ()
  => HasCallStack
  => MonadIO m
  => MonadTest m
  => ToJSON a
  => a
  -> m a
jotJson a =
  withFrozenCallStack do
    !b <- eval a
    jotWithCallStack GHC.callStack $ LT.unpack $ LT.decodeUtf8 $ J.encode b
    return b

-- | Annotate the given value as JSON.
jotJson_ :: forall a m. ()
  => HasCallStack
  => MonadIO m
  => MonadTest m
  => ToJSON a
  => a
  -> m ()
jotJson_ a =
  withFrozenCallStack do
    !b <- eval a
    jotWithCallStack GHC.callStack $ LT.unpack $ LT.decodeUtf8 $ J.encode b
    return ()

-- | Annotate the given value as JSON in a monadic context.
jotJsonM :: forall a m. ()
  => HasCallStack
  => MonadCatch m
  => MonadIO m
  => MonadTest m
  => ToJSON a
  => m a
  -> m a
jotJsonM a =
  withFrozenCallStack do
    !b <- evalM a
    jotWithCallStack GHC.callStack $ LT.unpack $ LT.decodeUtf8 $ J.encode b
    return b

-- | Annotate the given value as JSON in a monadic context.
jotJsonM_ :: forall a m. ()
  => HasCallStack
  => MonadCatch m
  => MonadIO m
  => MonadTest m
  => ToJSON a
  => m a
  -> m ()
jotJsonM_ a =
  withFrozenCallStack do
    !b <- evalM a
    jotWithCallStack GHC.callStack $ LT.unpack $ LT.decodeUtf8 $ J.encode b
    return ()

-- | Annotate the given value as JSON.
jotJsonPretty :: forall a m. ()
  => HasCallStack
  => MonadIO m
  => MonadTest m
  => ToJSON a
  => a
  -> m a
jotJsonPretty a =
  withFrozenCallStack do
    !b <- eval a
    jotWithCallStack GHC.callStack $ LT.unpack $ LT.decodeUtf8 $ J.encodePretty b
    return b

-- | Annotate the given value as JSON.
jotJsonPretty_ :: forall a m. ()
  => HasCallStack
  => MonadIO m
  => MonadTest m
  => ToJSON a
  => a
  -> m ()
jotJsonPretty_ a =
  withFrozenCallStack do
    !b <- eval a
    jotWithCallStack GHC.callStack $ LT.unpack $ LT.decodeUtf8 $ J.encodePretty b
    return ()

-- | Annotate the given value as JSON in a monadic context.
jotJsonPrettyM :: forall a m. ()
  => HasCallStack
  => MonadCatch m
  => MonadIO m
  => MonadTest m
  => ToJSON a
  => m a
  -> m a
jotJsonPrettyM a =
  withFrozenCallStack do
    !b <- evalM a
    jotWithCallStack GHC.callStack $ LT.unpack $ LT.decodeUtf8 $ J.encodePretty b
    return b

-- | Annotate the given value as JSON in a monadic context.
jotJsonPrettyM_ :: forall a m. ()
  => HasCallStack
  => MonadCatch m
  => MonadIO m
  => MonadTest m
  => ToJSON a
  => m a
  -> m ()
jotJsonPrettyM_ a =
  withFrozenCallStack do
    !b <- evalM a
    jotWithCallStack GHC.callStack $ LT.unpack $ LT.decodeUtf8 $ J.encodePretty b
    return ()

-- | Annotate the given value as JSON.
jotYaml :: forall a m. ()
  => HasCallStack
  => MonadIO m
  => MonadTest m
  => ToJSON a
  => a
  -> m a
jotYaml a =
  withFrozenCallStack do
    !b <- eval a
    jotWithCallStack GHC.callStack $ T.unpack $ T.decodeUtf8 $ Y.encode b
    return b

-- | Annotate the given value as JSON.
jotYaml_ :: forall a m. ()
  => HasCallStack
  => MonadIO m
  => MonadTest m
  => ToJSON a
  => a
  -> m ()
jotYaml_ a =
  withFrozenCallStack do
    !b <- eval a
    jotWithCallStack GHC.callStack $ T.unpack $ T.decodeUtf8 $ Y.encode b
    return ()

-- | Annotate the given value as JSON in a monadic context.
jotYamlM :: forall a m. ()
  => HasCallStack
  => MonadCatch m
  => MonadIO m
  => MonadTest m
  => ToJSON a
  => m a
  -> m a
jotYamlM a =
  withFrozenCallStack do
    !b <- evalM a
    jotWithCallStack GHC.callStack $ T.unpack $ T.decodeUtf8 $ Y.encode b
    return b

-- | Annotate the given value as JSON in a monadic context.
jotYamlM_ :: forall a m. ()
  => HasCallStack
  => MonadCatch m
  => MonadIO m
  => MonadTest m
  => ToJSON a
  => m a
  -> m ()
jotYamlM_ a =
  withFrozenCallStack do
    !b <- evalM a
    jotWithCallStack GHC.callStack $ T.unpack $ T.decodeUtf8 $ Y.encode b
    return ()

-- | Annotate the each value in the given traversable.
jotEach :: forall a m f. ()
  => HasCallStack
  => MonadIO m
  => MonadTest m
  => Show a
  => Traversable f
  => f a
  -> m (f a)
jotEach as =
  withFrozenCallStack do
    for_ as $ jotWithCallStack GHC.callStack . show
    return as

-- | Annotate the each value in the given traversable returning unit.
jotEach_ :: forall a m f. ()
  => HasCallStack
  => MonadIO m
  => MonadTest m
  => Show a
  => Traversable f
  => f a
  -> m ()
jotEach_ as =
  withFrozenCallStack $ for_ as $ jotWithCallStack GHC.callStack . show

-- | Annotate the each value in the given traversable in a monadic context.
jotEachM :: forall a m f. ()
  => HasCallStack
  => MonadIO m
  => MonadTest m
  => Show a
  => Traversable f
  => m (f a)
  -> m (f a)
jotEachM f =
  withFrozenCallStack do
    !as <- f
    for_ as $ jotWithCallStack GHC.callStack . show
    return as

-- | Annotate the each value in the given traversable in a monadic context returning unit.
jotEachM_ :: forall a m f. ()
  => HasCallStack
  => MonadIO m
  => MonadTest m
  => Show a
  => Traversable f
  => m (f a)
  -> m ()
jotEachM_ f =
  withFrozenCallStack do
    !as <- f
    for_ as $ jotWithCallStack GHC.callStack . show

-- | Annotate the each value in the given traversable in IO.
jotEachIO :: forall a m f. ()
  => HasCallStack
  => MonadIO m
  => MonadTest m
  => Show a
  => Traversable f
  => IO (f a)
  -> m (f a)
jotEachIO f =
  withFrozenCallStack do
    !as <- evalIO f
    for_ as $ jotWithCallStack GHC.callStack . show
    return as

-- | Annotate the each value in the given traversable in IO returning unit.
jotEachIO_ :: forall a m f. ()
  => HasCallStack
  => MonadIO m
  => MonadTest m
  => Show a
  => Traversable f
  => IO (f a)
  -> m ()
jotEachIO_ f =
  withFrozenCallStack do
    !as <- evalIO f
    for_ as $ jotWithCallStack GHC.callStack . show

-- | Return the input file path after annotating it relative to the package directory
jotPkgInputFile :: forall r. ()
  => HasCallStack
  => r <: Concurrent
  => r <: Error Failure
  => r <: Hedgehog
  => r <: Reader PackagePath
  => FilePath
  -> Eff r FilePath
jotPkgInputFile fp = withFrozenCallStack $ do
  PackagePath { filePath = pkgPath } <- ask
  jotString_ $ pkgPath <> "/" <> fp
  return fp

-- | Return the golden file path after annotating it relative to the package directory
jotPkgGoldenFile :: forall r. ()
  => HasCallStack
  => r <: Concurrent
  => r <: Error Failure
  => r <: Hedgehog
  => r <: Reader PackagePath
  => FilePath
  -> Eff r FilePath
jotPkgGoldenFile fp = withFrozenCallStack $ do
  PackagePath { filePath = pkgPath } <- ask
  jotString_ $ pkgPath <> "/" <> fp
  return fp

jotRootInputFile :: forall r. ()
  => HasCallStack
  => r <: Concurrent
  => r <: Error Failure
  => r <: Hedgehog
  => r <: Reader ProjectRoot
  => FilePath
  -> Eff r FilePath
jotRootInputFile fp = withFrozenCallStack $ do
  ProjectRoot { filePath = pkgPath } <- ask
  jotString $ pkgPath <> "/" <> fp

-- | Return the test file path after annotating it relative to the project root directory
jotTempFile :: forall r. ()
  => HasCallStack
  => r <: Concurrent
  => r <: Error Failure
  => r <: Hedgehog
  => r <: Reader Workspace
  => FilePath
  -> Eff r FilePath
jotTempFile fp = withFrozenCallStack $ do
  Workspace { filePath = workspace } <- ask
  let relPath = workspace <> "/" <> fp
  jotString_ $ workspace <> "/" <> relPath
  return relPath

jotLogTextWithCallStack :: forall m. ()
  => HasCallStack
  => MonadTest m
  => CallStack
  -> Severity
  -> Text
  -> m ()
jotLogTextWithCallStack cs severity a =
  withFrozenCallStack do
    jotWithCallStack cs $ T.unpack $ "[" <> toText severity <> "] " <> a

jotShowDataLog :: forall i a r. ()
  => HasCallStack
  => Show i
  => r <: Concurrent
  => r <: Error Failure
  => r <: Hedgehog
  => Eff (DataLog i : r) a
  -> Eff r a
jotShowDataLog =
  withFrozenCallStack $
    DataLog.runDataLog jotShow_
{-# inline jotShowDataLog #-}

writeLog :: forall m. ()
  => HasCallStack
  => MonadTest m
  => H.Log
  -> m ()
writeLog message =
  withFrozenCallStack $
    H.writeLog message
