{-# LANGUAGE MultiWayIf #-}

module Effectful.Zoo.Hedgehog.Golden
  ( diffVsGoldenFile,
    diffFileVsGoldenFile,
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Algorithm.Diff (PolyDiff (Both), getGroupedDiff)
import           Data.Algorithm.DiffOutput (ppDiff)
import           Data.Bool
import           Data.Eq
import           Data.Function
import           Data.Maybe
import           Data.Monoid
import           Data.String
import           Effectful
import           Effectful.Concurrent.QSem
import           Effectful.Exception
import           Effectful.Zoo.Core
import           Effectful.Zoo.Error.Static
import           Effectful.Zoo.FileSystem
import           Effectful.Zoo.Hedgehog.Api.Assert
import           Effectful.Zoo.Hedgehog.Api.Journal
import           Effectful.Zoo.Hedgehog.Effect.Hedgehog
import           Effectful.Zoo.Log.Static
import           GHC.Stack (callStack)
import           HaskellWorks.Prelude
import           Hedgehog (MonadTest)
import           System.FilePath (takeDirectory)
import qualified Control.Concurrent.QSem as IO
import qualified System.Environment as IO
import qualified System.IO.Unsafe as IO

import qualified Data.List as List
import qualified Data.Text as T
import qualified GHC.Stack as GHC
import qualified Hedgehog.Internal.Property as H

sem :: IO.QSem
sem = IO.unsafePerformIO $ IO.newQSem 1
{-# NOINLINE sem #-}

semBracket :: ()
  => r <: Concurrent
  => Eff r a
  -> Eff r a
semBracket = bracket_ (waitQSem sem) (signalQSem sem)

-- | The file to log whenever a golden file is referenced.
mGoldenFileLogFile :: Maybe FilePath
mGoldenFileLogFile = IO.unsafePerformIO $
  IO.lookupEnv "GOLDEN_FILE_LOG_FILE"

-- | Whether the test should create the golden files if the files do not exist.
createGoldenFiles :: Bool
createGoldenFiles = IO.unsafePerformIO $ do
  value <- IO.lookupEnv "CREATE_GOLDEN_FILES"
  return $ value == Just "1"

-- | Whether the test should recreate the golden files if the files already exist.
recreateGoldenFiles :: Bool
recreateGoldenFiles = IO.unsafePerformIO $ do
  value <- IO.lookupEnv "RECREATE_GOLDEN_FILES"
  return $ value == Just "1"

writeGoldenFile :: ()
  => HasCallStack
  => r <: Concurrent
  => r <: Error Failure
  => r <: Error IOException
  => r <: FileSystem
  => r <: Hedgehog
  => r <: Log Text
  => FilePath
  -> String
  -> Eff r ()
writeGoldenFile goldenFile actualContent = GHC.withFrozenCallStack $ do
  jot_ $ "Creating golden file " <> T.pack goldenFile
  void $ createDirectoryIfMissing True (takeDirectory goldenFile)
  writeStringFile goldenFile actualContent

reportGoldenFileMissing :: ()
  => HasCallStack
  => MonadIO m
  => MonadTest m
  => FilePath
  -> m ()
reportGoldenFileMissing goldenFile = GHC.withFrozenCallStack $ do
  jot_ $ T.unlines
    [ "Golden file " <> T.pack goldenFile <> " does not exist."
    , "To create it, run with CREATE_GOLDEN_FILES=1."
    , "To recreate it, run with RECREATE_GOLDEN_FILES=1."
    ]
  H.failure

checkAgainstGoldenFile :: ()
  => HasCallStack
  => r <: Concurrent
  => r <: Error Failure
  => r <: Error IOException
  => r <: FileSystem
  => r <: Hedgehog
  => r <: Log Text
  => FilePath
  -> [String]
  -> Eff r ()
checkAgainstGoldenFile goldenFile actualLines = GHC.withFrozenCallStack $ do
  referenceLines <- List.lines <$> readStringFile goldenFile
  let difference = getGroupedDiff actualLines referenceLines
  case difference of
    []       -> pure ()
    [Both{}] -> pure ()
    _        -> do
      jot_ $ T.unlines
        [ "Golden test failed against the golden file."
        , "To recreate golden file, run with RECREATE_GOLDEN_FILES=1."
        ]
      failMessage callStack $ ppDiff difference

-- | Diff contents against the golden file.  If CREATE_GOLDEN_FILES environment is
-- set to "1", then should the golden file not exist it would be created.  If
-- RECREATE_GOLDEN_FILES is set to "1", then should the golden file exist it would
-- be recreated. If GOLDEN_FILE_LOG_FILE is set to a filename, then the golden file
-- path will be logged to the specified file.
--
-- Set the environment variable when you intend to generate or re-generate the golden
-- file for example when running the test for the first time or if the golden file
-- genuinely needs to change.
--
-- To re-generate a golden file you must also delete the golden file because golden
-- files are never overwritten.
--
-- TODO: Improve the help output by saying the difference of
-- each input.
diffVsGoldenFile
  :: HasCallStack
  => r <: Concurrent
  => r <: Error Failure
  => r <: Error IOException
  => r <: FileSystem
  => r <: Log Text
  => r <: Hedgehog
  => r <: IOE
  => String   -- ^ Actual content
  -> FilePath -- ^ Reference file
  -> Eff r ()
diffVsGoldenFile actualContent goldenFile = GHC.withFrozenCallStack $ do
  forM_ mGoldenFileLogFile $ \logFile ->
    semBracket $ appendStringFile logFile $ goldenFile <> "\n"

  fileExists <- doesFileExist goldenFile

  if
    | recreateGoldenFiles -> writeGoldenFile goldenFile actualContent
    | fileExists          -> checkAgainstGoldenFile goldenFile actualLines
    | createGoldenFiles   -> writeGoldenFile goldenFile actualContent
    | otherwise           -> reportGoldenFileMissing goldenFile

  where
    actualLines = List.lines actualContent

-- | Diff file against the golden file.  If CREATE_GOLDEN_FILES environment is
-- set to "1", then should the gold file not exist it would be created.  If
-- GOLDEN_FILE_LOG_FILE is set to a filename, then the golden file path will be
-- logged to the specified file.
--
-- Set the environment variable when you intend to generate or re-generate the golden
-- file for example when running the test for the first time or if the golden file
-- genuinely needs to change.
--
-- To re-generate a golden file you must also delete the golden file because golden
-- files are never overwritten.
diffFileVsGoldenFile
  :: HasCallStack
  => r <: Concurrent
  => r <: Error Failure
  => r <: Error IOException
  => r <: FileSystem
  => r <: Hedgehog
  => r <: IOE
  => r <: Log Text
  => FilePath -- ^ Actual file
  -> FilePath -- ^ Reference file
  -> Eff r ()
diffFileVsGoldenFile actualFile referenceFile = GHC.withFrozenCallStack $ do
  contents <- readStringFile actualFile
  diffVsGoldenFile contents referenceFile
