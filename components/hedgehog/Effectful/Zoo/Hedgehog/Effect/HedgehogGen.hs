{- HLINT ignore "Eta reduce" -}

{-# OPTIONS_GHC -Wno-orphans #-}

module Effectful.Zoo.Hedgehog.Effect.HedgehogGen
  ( HedgehogGen,
    HedgehogGenEnv(..),
    askHedgehogGenEnv,
    runHedgehogGenProperty,
  ) where

import Effectful
import Effectful.Concurrent.STM
import Effectful.Dispatch.Static
import Effectful.Zoo.Core
import HaskellWorks.Prelude
import Hedgehog qualified as H

-- | An effect for interacting with the filesystem.
data HedgehogGen :: Effect

type instance DispatchOf HedgehogGen = Static WithSideEffects
newtype instance StaticRep HedgehogGen = HedgehogGen HedgehogGenEnv

newtype HedgehogGenEnv
  = HedgehogGenEnv (TMVar (H.PropertyT IO ()))

askHedgehogGenEnv :: ()
  => r <: HedgehogGen
  => Eff r HedgehogGenEnv
askHedgehogGenEnv = do
  HedgehogGen env <- getStaticRep
  pure env

runHedgehogGenProperty :: ()
  => r <: IOE
  => TMVar (H.PropertyT IO ())
  -> Eff (HedgehogGen : r) a
  -> Eff r a
runHedgehogGenProperty tvAction =
  evalStaticRep (HedgehogGen (HedgehogGenEnv tvAction))
