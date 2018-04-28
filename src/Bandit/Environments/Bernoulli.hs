{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Bandit.Environments.Bernoulli where

import Data.Map.Strict (Map, (!?))
import Data.Map.Strict as Map
import Data.Random.Distribution.Bernoulli

import Bandit.Environments.Types


data BernoulliEnv act = BernoulliEnv
  { expectedRewards :: Map act Double
  }

instance Ord act => Environment (BernoulliEnv act) () act Bool where
  generateReward (BernoulliEnv ps) _ctx act =
    bernoulli <$> ps !? act
  generateContext _ = pure ()


