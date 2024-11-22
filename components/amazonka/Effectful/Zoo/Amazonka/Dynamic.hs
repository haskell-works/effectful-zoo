{-# OPTIONS_GHC -fno-warn-orphans #-}

module Effectful.Zoo.Amazonka.Dynamic
  ( Amazonka(..),

    runAmazonka,
  ) where

import Amazonka qualified as AWS
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Resource
import Effectful.Zoo.Amazonka.Data.AwsEnv
import Effectful.Zoo.Amazonka.Data.AwsError
import Effectful.Zoo.Amazonka.Data.AwsRequest
import Effectful.Zoo.Amazonka.Data.AwsResponse
import Effectful.Zoo.Amazonka.Static qualified as S
import Effectful.Zoo.Core
import HaskellWorks.Prelude

data Amazonka :: Effect where
  AskAwsEnv
    :: Amazonka m AwsEnv

  LocalAwsEnv
    :: (AwsEnv -> AwsEnv)
    -> m a
    -> Amazonka m a
    
  SendAws
    :: ( HasCallStack
       , AwsRequest a
       , Typeable a
       , Typeable (AwsResponse a)
       )
    => a
    -> Amazonka m (Either AwsError (AwsResponse a))
  
type instance DispatchOf Amazonka = Dynamic

runAmazonka :: forall a r. ()
  => r <: IOE
  => r <: Resource
  => AWS.Env
  -> Eff (Amazonka : r) a
  -> Eff r a
runAmazonka awsEnv =
  reinterpret (S.runAmazonka awsEnv) $ \env -> \case
    AskAwsEnv -> do
      pure awsEnv
    LocalAwsEnv f m -> do
      localSeqUnlift env $ \unlift -> S.localAmazonka f (unlift m)
    SendAws req -> do
      AWS.sendEither awsEnv req