{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bandit.Environments.Types where

import Control.Monad.Writer
import Control.Monad.Trans (liftIO, MonadIO)
import qualified Control.Monad.Trans as T
import qualified Data.Random.Lift as R
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Random

import Bandit.Types


newtype Log rew = Log (Seq (rew, rew))
  deriving Monoid

cumulativeRegret :: Num rew => Log rew -> Seq rew
cumulativeRegret = Seq.scanl (+) 0 . perPeriodRegret

averageRegret :: (Fractional rew, Num rew)
              => Int -> Log rew -> Seq rew
averageRegret k log =
  let xs = cumulativeRegret log
      (h, t) = Seq.splitAt k xs
      totalRegret = Seq.scanl (+) (sum h) $ Seq.zipWith (-) t xs
  in (/ (fromIntegral k)) <$> totalRegret

perPeriodRegret :: Num rew => Log rew -> Seq rew
perPeriodRegret (Log seq) = uncurry (-) `fmap` seq

type ExperimentT rew m a = WriterT (Log rew) (RVarT m) a

logRewards :: rew -> rew -> ExperimentT rew m ()
logRewards opt actual = tell $ Log $ Seq.singleton (opt, actual)

liftRVar :: RVar a -> ExperimentT rew m a
liftRVar = lift . R.lift

class Environment env ctx act rew | env -> ctx act rew where
  generateReward :: env -> ctx -> act -> Maybe (RVar rew)
  generateContext :: env -> RVar ctx
  maximalReward :: env -> ctx -> rew
  expectedReward :: env -> ctx -> act -> rew

runExperiment :: MonadRandom m => ExperimentT rew m a -> m (a, Log rew)
runExperiment = sample . runWriterT

runMany :: MonadRandom m => Int -> ExperimentT rew m a -> m [Log rew]
runMany samples experiment = replicateM samples (snd <$> runExperiment experiment)

newExperiment :: (Monad m,
                  BanditAgent agent ctx act rew,
                  Environment env ctx act rew)
              => agent -> env -> Int -> ExperimentT rew m agent
newExperiment agent env trials = do
  if trials > 0 then
    do
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
        Nothing ->
          error "environment failed to generate a reward"
    else pure agent



