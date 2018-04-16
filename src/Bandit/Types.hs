{-# LANGUAGE TypeFamilies #-}
module Bandit.Types where

import Data.Random

class BanditAgent agent where
  type Reward agent
  type Action agent
  selectAction :: agent -> RVar (Action agent)
  updateAgent :: Action agent -> Reward agent -> agent -> agent


class SequentialEstimator estimator where
  type Estimate estimator
  type Observation estimator
  estimate :: estimator -> Estimate estimator
  updateEstimate :: Observation estimator -> estimator -> estimator
