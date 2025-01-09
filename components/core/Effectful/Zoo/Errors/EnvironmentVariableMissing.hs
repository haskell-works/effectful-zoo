module Effectful.Zoo.Errors.EnvironmentVariableMissing
  ( EnvironmentVariableMissing (..),
  ) where

import Data.Aeson (ToJSON (..), (.=))
import Data.Aeson qualified as J
import GHC.Generics
import HaskellWorks.Prelude

newtype EnvironmentVariableMissing = EnvironmentVariableMissing
    { variable :: Text
    }
    deriving stock (Eq, Show, Generic)

instance ToJSON EnvironmentVariableMissing where
    toJSON e =
        J.object
            [ "error" .= id @Text "EnvironmentVariableMissing"
            , "variable" .= e.variable
            ]
