module Effectful.Zoo.Hedgehog.Api.Gen.Time
  ( genPosixTime,
  ) where

import Data.Time.Clock.POSIX (POSIXTime)
import HaskellWorks.Prelude
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

genPosixTime :: Gen POSIXTime
genPosixTime = do
  -- Generate a random integer within a reasonable range for POSIX time
  -- POSIXTime is a type synonym for NominalDiffTime, which is in seconds
  -- We'll use a range from 0 to a large number of seconds to cover a wide time span
  seconds <- Gen.integral (Range.linear 0 4_102_444_800) -- Up to year 2100

  pure $ fromIntegral @Word64 seconds
