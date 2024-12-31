module Effectful.Zoo.FileSystem
  ( JsonDecodeError(..),
    YamlDecodeError(..),
    readByteStringFile,
    readLazyByteStringFile,
    readJsonFile,
    readYamlFile,
  ) where

import Data.Aeson (FromJSON)
import Data.Aeson qualified as J
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Yaml qualified as Y
import Effectful
import Effectful.Dispatch.Static
import Effectful.FileSystem
import Effectful.Zoo.Core
import Effectful.Zoo.Core.Error.Static
import Effectful.Zoo.Core.Exception
import Effectful.Zoo.Log.Api.Text
import Effectful.Zoo.Log.Dynamic
import Effectful.Zoo.Unsafe
import HaskellWorks.Error.Types.JsonDecodeError
import HaskellWorks.Error.Types.YamlDecodeError
import HaskellWorks.Prelude

readLazyByteStringFile :: ()
  => HasCallStack
  => r <: Error IOException
  => r <: FileSystem
  => r <: Log Text
  => FilePath
  -> Eff r LBS.ByteString
readLazyByteStringFile filePath = withFrozenCallStack $ do
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
readByteStringFile filePath = withFrozenCallStack $ do
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
readJsonFile filePath = withFrozenCallStack $ do
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
readYamlFile filePath = withFrozenCallStack $ do
  info $ "Reading YAML file: " <> T.pack filePath
  contents <- LBS.toStrict <$> readLazyByteStringFile filePath
  Y.decodeEither' contents
    & onLeft (throw . YamlDecodeError . T.pack . Y.prettyPrintParseException)
