module Effectful.Zoo.Errors.EnvironmentVariableInvalid
  ( EnvironmentVariableInvalid (..),
  ) where

import Data.Aeson (ToJSON (..), (.=))
import Data.Aeson qualified as J
import GHC.Generics
import HaskellWorks.Prelude

data EnvironmentVariableInvalid = EnvironmentVariableInvalid
    { variable :: Text
    , text :: Text
    , reason :: Maybe Text
    }
    deriving stock (Eq, Show, Generic)

instance ToJSON EnvironmentVariableInvalid where
    toJSON e =
        J.object
            [ "error" .= id @Text "EnvironmentVariableInvalid"
            , "variable" .= e.variable
            , "text" .= e.text
            , "reason" .= e.reason
            ]
