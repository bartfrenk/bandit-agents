{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Bandit.Environments.Types where

import Control.Monad.Trans (liftIO, MonadIO)
import qualified Control.Monad.Trans as T
import qualified Data.Random.Lift as R
import Data.Random

import Bandit.Types


class Environment env ctx act rew | env -> ctx act rew where
  generateReward :: env -> ctx -> act -> Maybe (RVar rew)
  generateContext :: env -> RVar ctx


newExperiment :: (MonadIO m,
                  BanditAgent agent ctx act rew,
                  Environment env ctx act rew)
              => agent -> env -> Int -> RVarT m agent
newExperiment agent env trials = do
  if trials > 0 then
    do
      ctx <- R.lift $ generateContext env
      act <- R.lift $ selectAction agent ctx
      case R.lift $ generateReward env ctx act of
        Just rew' -> do
          rew <- R.lift rew'
          let agent' = updateBelief ctx act rew agent
          newExperiment agent' env (trials - 1)
        Nothing ->
          error "environment failed to generate a reward"
    else pure agent

