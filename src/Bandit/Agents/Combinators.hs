{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}

module Bandit.Agents.Combinators where

import qualified Data.Random.Distribution.Bernoulli as D

import           Bandit.Agents.Types

data MixedBandit ctx act rew =
  forall b a. (BanditAgent a ctx act rew, BanditAgent b ctx act rew) =>
              MixedBandit Double a b

instance Eq act => BanditAgent (MixedBandit ctx act rew) ctx act rew where
  selectAction (MixedBandit p a b) ctx = do
    choice <- D.bernoulli p
    if choice
      then selectAction a ctx
      else selectAction b ctx
  updateBelief ctx act rew (MixedBandit p a b) =
    MixedBandit p (updateBelief ctx act rew a) (updateBelief ctx act rew b)

-- | Creates an agent that selects an action according to `a` with probability
-- `p` and according to `b` with probability `1 - p`.
newMixedAgent ::
     (BanditAgent a ctx act rew, BanditAgent b ctx act rew)
  => Double
  -> a
  -> b
  -> MixedBandit ctx act rew
newMixedAgent p a b =
  if 0 <= p && p <= 1
    then MixedBandit p a b
    else error "MixedBandit: probability needs to be between 0 and 1"
