{- HLINT ignore "Eta reduce" -}

{-# OPTIONS_GHC -Wno-orphans #-}

module Effectful.Zoo.Hedgehog.Effect.Run
  ( Hedgehog,
    property,
    unit,
    forAll,
  ) where

import Control.Concurrent qualified as IO
import Control.Concurrent.STM qualified as IO
import Control.Exception qualified as IO
import Control.Exception.Lifted qualified as CEL
import Control.Monad
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.STM
import Effectful.Concurrent.STM qualified as CC
import Effectful.Environment
import Effectful.FileSystem
import Effectful.Zoo.Core
import Effectful.Zoo.Error.Static
import Effectful.Zoo.Hedgehog.Api.Failure
import Effectful.Zoo.Hedgehog.Api.Journal
import Effectful.Zoo.Hedgehog.Api.MonadAssertion
import Effectful.Zoo.Hedgehog.Effect.Hedgehog
import Effectful.Zoo.Hedgehog.Effect.HedgehogGen
import Effectful.Zoo.Log.Data.LogMessage
import Effectful.Zoo.Log.Dynamic
import Effectful.Zoo.Resource
import HaskellWorks.Control.Monad
import HaskellWorks.Prelude
import HaskellWorks.ToText
import Hedgehog (Gen)
import Hedgehog qualified as H
import Hedgehog.Internal.Property qualified as H

property :: ()
  => Eff
      [ Log Text
      , HedgehogGen
      , Hedgehog
      , Error H.Failure
      , Resource
      , FileSystem
      , Environment
      , Concurrent
      , IOE
      ] ()
  -> H.PropertyT IO ()
property f = do
  tvResult <- liftIO IO.newEmptyTMVarIO
  tvAction <- liftIO IO.newEmptyTMVarIO
  CEL.bracket
    do  liftIO $ IO.forkFinally
          do  f
                & runLog @Text (ConcUnlift Persistent Unlimited) logTextToHedgehog
                & runHedgehogGenProperty tvAction
                & runHedgehogProperty tvAction
                & runError @H.Failure
                & runResource
                & runFileSystem
                & runEnvironment
                & runConcurrent
                & runEff
          (liftIO . IO.atomically . IO.putTMVar tvResult)
    do liftIO . IO.killThread
    do \_ -> do
        whileNothingM do
          mActionOrResult <- liftIO $ IO.atomically do
            mAction <- CC.tryTakeTMVar tvAction
            case mAction of
              Nothing -> do
                mResult <- CC.tryTakeTMVar tvResult
                case mResult of
                  Just a -> pure $ Just $ Right a
                  Nothing -> retry
              Just action -> pure $ Just $ Left action

          case mActionOrResult of
            Nothing -> pure Nothing
            Just (Left action) -> action >> pure Nothing
            Just (Right (Left e)) -> liftIO $ IO.throwIO e
            Just (Right (Right (Left (_, e)))) -> throwAssertion e
            Just (Right (Right (Right a))) -> pure $ Just a

logTextToHedgehog :: ()
  => r <: Concurrent
  => r <: Hedgehog
  => r <: Error Failure
  => CallStack
  -> LogMessage Text
  -> Eff r ()
logTextToHedgehog callStack msg = do
  let LogMessage severity text = msg
  jotTextWithCallStack callStack $ toText severity <> " " <> text

unit :: ()
  => Eff
      [ Log Text
      , Hedgehog
      , Error H.Failure
      , Resource
      , FileSystem
      , Environment
      , Concurrent
      , IOE
      ] ()
  -> H.TestT IO ()
unit f = do
  tvResult <- liftIO IO.newEmptyTMVarIO
  tvAction <- liftIO IO.newEmptyTMVarIO
  CEL.bracket
    do  liftIO $ IO.forkFinally
          do  f & runLog @Text (ConcUnlift Persistent Unlimited) logTextToHedgehog
                & runHedgehogUnit tvAction
                & runError @H.Failure
                & runResource
                & runFileSystem
                & runEnvironment
                & runConcurrent
                & runEff
          (liftIO . IO.atomically . IO.putTMVar tvResult)
    do liftIO . IO.killThread
    do \_ -> do
        whileNothingM do
          mActionOrResult <- liftIO $ IO.atomically do
            mAction <- CC.tryTakeTMVar tvAction
            case mAction of
              Nothing -> do
                mResult <- CC.tryTakeTMVar tvResult
                case mResult of
                  Just a -> pure $ Just $ Right a
                  Nothing -> retry
              Just action -> pure $ Just $ Left action

          case mActionOrResult of
            Nothing -> pure Nothing
            Just (Left action) -> action >> pure Nothing
            Just (Right (Left e)) -> liftIO $ IO.throwIO e
            Just (Right (Right (Left (_, e)))) -> throwAssertion e
            Just (Right (Right (Right a))) -> pure $ Just a

forAll :: ()
  => r <: Concurrent
  => r <: HedgehogGen
  => Show a
  => Gen a
  -> Eff r a
forAll gen = do
  HedgehogGenEnv mvAction <- askHedgehogGenEnv
  mvA <- newEmptyTMVarIO
  atomically $ putTMVar mvAction (H.forAll gen >>= liftIO . IO.atomically . IO.putTMVar mvA)
  atomically $ takeTMVar mvA
