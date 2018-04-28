{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Bandit.Types where

import Data.Random (RVar)

class BanditAgent agent ctx act rew | agent -> act ctx rew where
  selectAction :: agent -> ctx -> RVar act
  updateBelief :: ctx -> act -> rew -> agent -> agent

class BatchUpdateBelief agent batch where
  batchUpdateBelief :: batch -> agent -> agent

class SequentialEstimator estimator est obs | estimator -> est obs where
  estimate :: estimator -> est
  updateEstimate :: obs -> estimator -> estimator


-- class CtxBanditAgent agent ctx action reward | agent -> ctx action reward where
--   selectActionFromCtx :: agent -> ctx -> RVar action
--   updateAgentWithCtx :: ctx -> action -> reward -> agent -> agent
