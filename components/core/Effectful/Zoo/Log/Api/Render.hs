module Effectful.Zoo.Log.Api.Render
  ( defaultRenderLogToText,
  ) where

import Effectful.Zoo.Log.Data.Severity
import HaskellWorks.Prelude
import HaskellWorks.ToText

defaultRenderLogToText :: ToText i => Severity -> i -> Text
defaultRenderLogToText severity i =
  "[" <> toText severity <> "] " <> toText i
