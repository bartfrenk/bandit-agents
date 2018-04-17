{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Bandit.Types where

import Data.Random

class BanditAgent agent action reward | agent -> action reward where
  selectAction :: agent -> RVar action
  updateAgent :: action -> reward -> agent -> agent


class SequentialEstimator estimator est obs | estimator -> est obs where
  estimate :: estimator -> est
  updateEstimate :: obs -> estimator -> estimator


class CtxBanditAgent agent ctx action reward | agent -> ctx action reward where
  selectActionFromCtx :: agent -> ctx -> RVar action
  updateAgentWithCtx :: ctx -> action -> reward -> agent -> agent
