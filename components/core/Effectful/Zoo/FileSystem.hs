module Effectful.Zoo.FileSystem
  ( FileSystem,
    JsonDecodeError(..),
    YamlDecodeError(..),
    readByteStringFile,
    readLazyByteStringFile,
    readStringFile,
    readJsonFile,
    readYamlFile,
    writeStringFile,
    getCanonicalTemporaryDirectory,
    createTempDirectory,
    removePathForcibly,
    doesFileExist,
    doesDirectoryExist,

    runFileSystem,

    getCurrentDirectory,
    canonicalizePath,
  ) where

import Data.Aeson (FromJSON)
import Data.Aeson qualified as J
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Yaml qualified as Y
import Effectful
import Effectful.Dispatch.Static
import Effectful.FileSystem (FileSystem, runFileSystem)
import Effectful.Zoo.Core
import Effectful.Zoo.Error.Static
import Effectful.Zoo.Exception
import Effectful.Zoo.Log.Api.Text
import Effectful.Zoo.Log.Dynamic
import Effectful.Zoo.Unsafe
import HaskellWorks.Error.Types.JsonDecodeError
import HaskellWorks.Error.Types.YamlDecodeError
import HaskellWorks.Prelude
import System.Directory qualified as D
import System.IO qualified as IO
import System.IO.Temp qualified as IO

readStringFile :: ()
  => HasCallStack
  => r <: Error IOException
  => r <: FileSystem
  => r <: Log Text
  => FilePath
  -> Eff r String
readStringFile filePath = withFrozenCallStack do
  info $ "Reading bytestring from file: " <> T.pack filePath
  unsafeFileSystemEff_ (IO.readFile filePath)
    & trapIO @IOException throw

readLazyByteStringFile :: ()
  => HasCallStack
  => r <: Error IOException
  => r <: FileSystem
  => r <: Log Text
  => FilePath
  -> Eff r LBS.ByteString
readLazyByteStringFile filePath = withFrozenCallStack do
  info $ "Reading bytestring from file: " <> T.pack filePath
  unsafeFileSystemEff_ (LBS.readFile filePath)
    & trapIO @IOException throw

readByteStringFile :: ()
  => HasCallStack
  => r <: Error IOException
  => r <: FileSystem
  => r <: Log Text
  => FilePath
  -> Eff r ByteString
readByteStringFile filePath = withFrozenCallStack do
  info $ "Reading bytestring from file: " <> T.pack filePath
  unsafeFileSystemEff_ (BS.readFile filePath)
    & trapIO @IOException throw

-- | Read the 'filePath' file as JSON. Use @readJsonFile \@'Value'@ to decode into 'Value'.
readJsonFile :: forall a r. ()
  => FromJSON a
  => HasCallStack
  => r <: Error IOException
  => r <: Error JsonDecodeError
  => r <: FileSystem
  => r <: Log Text
  => FilePath
  -> Eff r a
readJsonFile filePath = withFrozenCallStack do
  info $ "Reading JSON file: " <> T.pack filePath
  contents <- readLazyByteStringFile filePath
  J.eitherDecode contents
    & onLeft (throw . newJsonDecodeError)

-- | Read the 'filePath' file as YAML.
readYamlFile :: forall a r. ()
  => FromJSON a
  => HasCallStack
  => r <: Error IOException
  => r <: Error YamlDecodeError
  => r <: FileSystem
  => r <: Log Text
  => FilePath
  -> Eff r a
readYamlFile filePath = withFrozenCallStack do
  info $ "Reading YAML file: " <> T.pack filePath
  contents <- LBS.toStrict <$> readLazyByteStringFile filePath
  Y.decodeEither' contents
    & onLeft (throw . YamlDecodeError . T.pack . Y.prettyPrintParseException)

writeStringFile :: ()
  => HasCallStack
  => r <: Error IOException
  => r <: FileSystem
  => r <: Log Text
  => FilePath
  -> String
  -> Eff r ()
writeStringFile filePath contents = withFrozenCallStack do
  info $ "Writing string to file: " <> T.pack filePath
  unsafeFileSystemEff_ (IO.writeFile filePath contents)
    & trapIO @IOException throw

getCanonicalTemporaryDirectory :: ()
  => HasCallStack
  => r <: Error IOException
  => r <: FileSystem
  => Eff r FilePath
getCanonicalTemporaryDirectory = withFrozenCallStack do
  unsafeFileSystemEff_ IO.getCanonicalTemporaryDirectory
    & trapIO @IOException throw
{-# INLINE getCanonicalTemporaryDirectory #-}

createTempDirectory :: ()
  => HasCallStack
  => r <: Error IOException
  => r <: FileSystem
  => FilePath
  -> String
  -> Eff r FilePath
createTempDirectory fp template = withFrozenCallStack do
  unsafeFileSystemEff_ (IO.createTempDirectory fp template)
    & trapIO @IOException throw
{-# INLINE createTempDirectory #-}

removePathForcibly :: ()
  => HasCallStack
  => r <: Error IOException
  => r <: FileSystem
  => r <: Log Text
  => FilePath
  -> Eff r ()
removePathForcibly fp = withFrozenCallStack $ do
  info $ "Calling: removePathForcibly " <> tshow fp

  unsafeFileSystemEff_ (D.removePathForcibly fp)
    & trapIO @IOException throw

doesFileExist :: ()
  => HasCallStack
  => r <: Error IOException
  => r <: FileSystem
  => r <: Log Text
  => FilePath
  -> Eff r Bool
doesFileExist fp = withFrozenCallStack $ do
  info "Calling: doesFileExist"

  unsafeFileSystemEff_ (D.doesFileExist fp)
    & trapIO @IOException throw

doesDirectoryExist :: ()
  => HasCallStack
  => r <: Error IOException
  => r <: FileSystem
  => r <: Log Text
  => FilePath
  -> Eff r Bool
doesDirectoryExist fp = withFrozenCallStack $ do
  info "Calling: doesDirectoryExist"

  unsafeFileSystemEff_ (D.doesDirectoryExist fp)
    & trapIO @IOException throw

getCurrentDirectory :: ()
  => HasCallStack
  => r <: Error IOException
  => r <: FileSystem
  => Eff r FilePath
getCurrentDirectory = withFrozenCallStack do
  unsafeFileSystemEff_ D.getCurrentDirectory
    & trapIO @IOException throw
{-# INLINE getCurrentDirectory #-}

canonicalizePath :: ()
  => HasCallStack
  => r <: Error IOException
  => r <: FileSystem
  => FilePath
  -> Eff r FilePath
canonicalizePath fp = withFrozenCallStack do
  unsafeFileSystemEff_ (D.canonicalizePath fp)
    & trapIO @IOException throw
{-# INLINE canonicalizePath #-}
