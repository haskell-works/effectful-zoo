module Effectful.Zoo.Hedgehog.Run
  ( runHedgehog,
    runHedgehogIO,
    interpretHedgehog,
  ) where

import Control.Monad.Trans.Except (ExceptT(..))
import Control.Monad.Trans.Writer.Lazy qualified as MTL
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Writer.Static.Local
import Effectful.Zoo.Core
import Effectful.Zoo.Hedgehog.Effect
import HaskellWorks.Prelude
import Hedgehog qualified as H
import Hedgehog.Internal.Property (Failure, Journal)
import Hedgehog.Internal.Property qualified as H
import Effectful.Zoo.Hedgehog.MonadTestProxy

runHedgehog :: forall a. ()
  => Eff [Hedgehog,  Error Failure, Writer Journal, IOE] a
  -> H.TestT IO a
runHedgehog f =
  f
    & runHedgehogIO
    & MTL.WriterT
    & ExceptT
    & H.TestT

runHedgehogIO :: forall a. ()
  => Eff [Hedgehog, Error Failure, Writer Journal, IOE] a
  -> IO (Either Failure a, Journal)
runHedgehogIO f =
  f & interpretHedgehog
    & runError @H.Failure
    & runWriter @H.Journal
    & fmap (first (first snd))
    & runEff

interpretHedgehog :: forall a r. ()
  => r <: Error Failure
  => r <: Writer Journal
  => Eff (Hedgehog : r) a
  -> Eff r a
interpretHedgehog =
  interpret $ \env -> \case
    CatchAssertion f h -> localUnlift env SeqUnlift $ \unlift -> catchError (unlift f) (const (unlift . h))
    LiftTest f -> liftTestProxy f
    ThrowAssertion failure -> throwError failure
