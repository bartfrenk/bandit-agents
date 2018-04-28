{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Bandit.Environments.Types where

import Data.Random

import Bandit.Types


class Environment env ctx act rew | env -> ctx act rew where
  generateReward :: env -> ctx -> act -> Maybe (RVar rew)
  generateContext :: env -> RVar ctx


newExperiment :: (BanditAgent agent ctx act rew,
                  Environment env ctx act rew)
              => agent -> env -> Int -> RVar agent
newExperiment agent env trials = do
  if trials > 0 then
    do
      ctx <- generateContext env
      act <- selectAction agent ctx
      case generateReward env ctx act of
        Just rew' -> do
          rew <- rew'
          let agent' = updateBelief ctx act rew agent
          newExperiment agent env (trials - 1)
        Nothing ->
          error "environment failed to generate a reward"
    else pure agent

