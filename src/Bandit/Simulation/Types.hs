{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Bandit.Simulation.Types where

import           Control.Monad
import           Control.Monad.Trans       (lift)
import           Control.Monad.Writer      (WriterT, runWriterT, tell)
import           Data.Random               (MonadRandom, RVar, RVarT)
import qualified Data.Random               as R
import qualified Data.Random.Lift          as R
import           Data.Sequence             (Seq)
import qualified Data.Sequence             as Seq

import           Bandit.Environments.Types
import           Bandit.Agents.Types

newtype Log rew =
  Log (Seq (rew, rew))
  deriving (Monoid)

type ExperimentT rew m a = WriterT (Log rew) (RVarT m) a

newExperiment ::
     (Monad m, BanditAgent agent ctx act rew, Environment env ctx act rew)
  => agent
  -> env
  -> Int
  -> ExperimentT rew m agent
newExperiment agent env trials = do
  if trials > 0
    then do
      ctx <- liftRVar $ generateContext env
      act <- liftRVar $ selectAction agent ctx
      case R.lift $ generateReward env ctx act of
        Just rew' -> do
          rew <- liftRVar rew'
          let optRew = maximalReward env ctx
          let expRew = expectedReward env ctx act
          logRewards optRew expRew
          let agent' = updateBelief ctx act rew agent
          newExperiment agent' env (trials - 1)
        Nothing -> error "environment failed to generate a reward"
    else pure agent

logRewards :: rew -> rew -> ExperimentT rew m ()
logRewards opt actual = tell $ Log $ Seq.singleton (opt, actual)

liftRVar :: RVar a -> ExperimentT rew m a
liftRVar = lift . R.lift

runExperiment :: MonadRandom m => ExperimentT rew m a -> m (a, Log rew)
runExperiment = R.sample . runWriterT

runMany :: MonadRandom m => Int -> ExperimentT rew m a -> m [Log rew]
runMany samples experiment =
  replicateM samples (snd <$> runExperiment experiment)
