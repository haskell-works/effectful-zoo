{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}

module Effectful.Zoo.RdsData.Errors.RdsDataError
  ( RdsDataError(..)
  ) where

import HaskellWorks.Prelude

data RdsDataError where
  EnvironmentVariableMissing
    :: String
    -> RdsDataError

  RdsDataError
    :: Text
    -> RdsDataError

deriving stock instance Show RdsDataError
