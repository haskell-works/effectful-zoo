module Effectful.Zoo.Hedgehog.Api.Eval
  ( eval,
    evalIO,
    evalM,

    evalEither,
    evalMaybe,
  ) where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Zoo.Core
import Effectful.Zoo.Hedgehog.Dynamic
import HaskellWorks.Prelude
import Hedgehog qualified as H

eval :: forall a r. ()
  => HasCallStack
  => r <: Hedgehog
  => a
  -> Eff r a
eval a =
  withFrozenCallStack do
    H.eval a

-- |Embeds 'Hedgehog.evalEither'.
evalEither :: forall a e r. ()
  => HasCallStack
  => r <: Hedgehog
  => Show e
  => Either e a
  -> Eff r a
evalEither e =
  withFrozenCallStack do
    H.evalEither e

-- |Embeds 'Hedgehog.evalMaybe'.
evalMaybe :: forall a r. ()
  => HasCallStack
  => r <: Hedgehog
  => Maybe a
  -> Eff r a
evalMaybe e =
  withFrozenCallStack do
    H.evalMaybe e

evalIO :: forall a r. ()
  => HasCallStack
  => r <: IOE
  => r <: Hedgehog
  => IO a
  -> Eff r a
evalIO f =
  withFrozenCallStack do
    H.evalIO f

evalM :: forall a r. ()
  => HasCallStack
  => r <: Hedgehog
  => Eff r a
  -> Eff r a
evalM f =
  withFrozenCallStack do
    H.evalM f
