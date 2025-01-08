module Effectful.Zoo.TestContainers.LocalStack.Types
  ( LocalStackEndpoint(..)
  ) where

import HaskellWorks.Prelude

data LocalStackEndpoint = LocalStackEndpoint
  { host :: String
  , port :: Int
  } deriving stock (Eq, Show, Generic)
