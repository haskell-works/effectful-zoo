module Effectful.Zoo.Aeson
  ( aesonDecode,
  ) where

import Data.Aeson (FromJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Effectful
import Effectful.Zoo.Core
import Effectful.Zoo.Core.Error.Static
import HaskellWorks.Error.Types.JsonDecodeError
import HaskellWorks.Prelude

aesonDecode :: forall a r. ()
  => r <: Error JsonDecodeError
  => FromJSON a
  => LBS.ByteString
  -> Eff r a
aesonDecode bs =
  fromEither (Aeson.eitherDecode bs)
    & mapError newJsonDecodeError
