{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}

module Bandit.Agents.Types where

import           Data.Random (RVar)

class BanditAgent agent ctx act rew | agent -> act ctx rew where
  selectAction :: agent -> ctx -> RVar act
  updateBelief :: ctx -> act -> rew -> agent -> agent

class BatchUpdateBelief agent batch where
  batchUpdateBelief :: batch -> agent -> agent

class SequentialEstimator estimator est obs | estimator -> est obs where
  estimate :: estimator -> est
  updateEstimate :: obs -> estimator -> estimator
