{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Bandit.Simulation.Types where

import           Control.Monad
import           Control.Monad.Par
import           Control.Monad.State       hiding (get)
import           Control.Monad.Trans       (lift)
import           Control.Monad.Writer      (WriterT, runWriterT, tell)
import           Data.Random               (MonadRandom, RVar, RVarT)
import qualified Data.Random               as R
import qualified Data.Random.Lift          as R
import           Data.Sequence             (Seq)
import qualified Data.Sequence             as Seq
import           System.Random

import           Bandit.Agents.Types
import           Bandit.Environments.Types

newtype Log rew =
  Log (Seq (rew, rew))
  deriving (Monoid, NFData)

type ExperimentT rew m a = WriterT (Log rew) (RVarT m) a

type Simulation agent env ctx act rew
   = (BanditAgent agent ctx act rew, Environment env ctx act rew)

newExperiment ::
     (Monad m, Simulation agent env ctx act rew)
  => agent
  -> env
  -> Int -- ^ The number of rounds to be played.
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

runPure :: StdGen -> ExperimentT rew (State StdGen) a -> (a, Log rew)
runPure gen exp = evalState (runExperiment exp) gen

logRewards :: rew -> rew -> ExperimentT rew m ()
logRewards opt actual = tell $ Log $ Seq.singleton (opt, actual)

liftRVar :: RVar a -> ExperimentT rew m a
liftRVar = lift . R.lift

runExperiment :: MonadRandom m => ExperimentT rew m a -> m (a, Log rew)
runExperiment = R.sample . runWriterT

runMany :: MonadRandom m => Int -> ExperimentT rew m a -> m [Log rew]
runMany samples experiment =
  replicateM samples (snd <$> runExperiment experiment)

-- TODO: Maybe parameterize by RandomGen constraint
runManyPure ::
     NFData rew
  => StdGen
  -> Int
  -> ExperimentT rew (State StdGen) a
  -> Par [Log rew]
runManyPure gen samples exp =
  let logs = (\g -> (snd $ runPure g exp)) `fmap` splitSequence samples gen
      g :: NFData rew => Log rew -> Par (Log rew)
      g log = spawnP log >>= get
  in g `traverse` logs

splitSequence :: Int -> StdGen -> [StdGen]
splitSequence = recur []
  where
    recur !acc n gen
      | n == 0 = acc
      | n > 0 =
        let (g1, g2) = split gen
        in recur (g1 : acc) (n - 1) g2
      | otherwise = []
