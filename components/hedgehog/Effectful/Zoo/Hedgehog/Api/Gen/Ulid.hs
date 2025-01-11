module Effectful.Zoo.Hedgehog.Api.Gen.Ulid (
    genUlid,
    genUlidRandom,
    genUlidTimeStamp,
) where

import Data.Binary (decodeOrFail)
import Data.ByteString.Lazy qualified as LBS
import Data.ULID (ULID (..))
import Data.ULID.Random (ULIDRandom)
import Data.ULID.TimeStamp (ULIDTimeStamp, mkULIDTimeStamp)
import Effectful.Zoo.Hedgehog.Api.Gen.Time
import HaskellWorks.Prelude
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

genUlidRandom :: Gen ULIDRandom
genUlidRandom = do
  bytes <- Gen.bytes (Range.singleton 10) -- 80 bits
  let lazyBytes = LBS.fromStrict bytes
  case decodeOrFail lazyBytes of
    Left (_, _, err)   -> fail $ "Failed to decode ULIDRandom: " <> err -- This shouldn't happen.
    Right (_, _, ulid) -> pure ulid

genUlidTimeStamp :: Gen ULIDTimeStamp
genUlidTimeStamp =
  mkULIDTimeStamp <$> genPosixTime

genUlid :: Gen ULID
genUlid =
  ULID <$> genUlidTimeStamp <*> genUlidRandom
