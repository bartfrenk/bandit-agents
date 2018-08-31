module Bandit.Environments.Bernoulli where

import           Data.Map.Strict                    (Map, (!), (!?))
import qualified Data.Map.Strict                    as Map
import           Data.Random.Distribution.Bernoulli

import           Bandit.Environments.Types

newBernoulliEnv :: Ord act => [(act, Double)] -> BernoulliEnv act
newBernoulliEnv = BernoulliEnv . Map.fromList

data BernoulliEnv act = BernoulliEnv
  { expectedRewards :: Map act Double
  }

instance Ord act => Environment (BernoulliEnv act) () act Double where
  generateReward (BernoulliEnv ps) _ctx act = bernoulli <$> ps !? act
  generateContext _ = pure ()
  maximalReward (BernoulliEnv ps) _ = Map.foldr max 0 ps
  expectedReward (BernoulliEnv ps) _ act = ps ! act
