{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Bandit.Combinators where

import qualified Data.Random.Distribution.Bernoulli as D

import Bandit.Types


data MixedBandit action reward
  = forall a b. (BanditAgent a action reward, BanditAgent b action reward)
  => MixedBandit Double a b


instance Eq action => BanditAgent (MixedBandit action reward) action reward where
  selectAction (MixedBandit p a b) = do
    choice <- D.bernoulli p
    if choice
      then selectAction a
      else selectAction b
  updateAgent action reward (MixedBandit p a b)
    = MixedBandit p (updateAgent action reward a) (updateAgent action reward b)


newMixedBandit :: (BanditAgent a action reward, BanditAgent b action reward)
               => Double -> a -> b -> MixedBandit action reward
newMixedBandit p a b = if 0 <= p && p <= 1
                       then MixedBandit p a b
                       else error "MixedBandit: probability needs to be between 0 and 1"




