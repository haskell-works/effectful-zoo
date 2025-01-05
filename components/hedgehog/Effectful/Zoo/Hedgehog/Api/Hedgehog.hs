module Effectful.Zoo.Hedgehog.Api.Hedgehog
  ( H.classify,

    H.eval,
    H.evalIO,
    H.evalM,

    H.evalEither,
    H.evalMaybe,

    H.assert,
    (H.===),
    (H./==),

    H.failure,
    H.failException,
    H.failWith,

  ) where

import Hedgehog qualified as H
import Hedgehog.Internal.Property qualified as H
