{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StrictData #-}

module Bandit.Bernoulli
  (newSequentialMean,
   newBanditTS,
   newBanditGreedy,
   newBanditRandom,
   newMixedBandit,
   newBanditEpsilonGreedy,
   MixedBandit,
   BernoulliBanditGreedy,
   BernoulliBanditRandom,
   BernoulliBanditTS) where


import Control.Monad
import Data.Random
import qualified Data.Random.Distribution.Beta as D
import qualified Data.Random.Distribution.Bernoulli as D

import Data.Random.Source.DevRandom

import Bandit.Types

-- |Selecting an action:
select :: IO Int
select = runRVar (selectAction $ newBanditRandom [1, 2, 3]) DevRandom


data MixedBandit action reward
  = forall a b. (BanditAgent a action reward, BanditAgent b action reward)
  => MixedBandit Double a b


newBanditEpsilonGreedy :: Eq action => Double -> [action] -> MixedBandit action Bool
newBanditEpsilonGreedy p actions
  = newMixedBandit p (newBanditRandom actions) (newBanditGreedy actions)


newMixedBandit :: (BanditAgent a action reward, BanditAgent b action reward)
               => Double -> a -> b -> MixedBandit action reward
newMixedBandit p a b = if 0 <= p && p <= 1
                       then MixedBandit p a b
                       else error "MixedBandit: probability needs to be between 0 and 1"


instance Eq action => BanditAgent (MixedBandit action reward) action reward where
  selectAction (MixedBandit p a b) = do
    choice <- D.bernoulli p
    if choice
      then selectAction a
      else selectAction b
  updateAgent action reward (MixedBandit p a b)
    = MixedBandit p (updateAgent action reward a) (updateAgent action reward b)


data BernoulliBanditRandom action = BernoulliBanditRandom [action]


newBanditRandom :: Eq action => [action] -> BernoulliBanditRandom action
newBanditRandom actions
  = if hasDuplicates actions
    then error "BernoulliBanditRandom: actions need to be unique"
    else BernoulliBanditRandom actions


instance Eq action => BanditAgent (BernoulliBanditRandom action) action Bool where
  selectAction (BernoulliBanditRandom actions) = randomElement actions
  updateAgent _ _ agent = agent


data BernoulliBanditGreedy action = BernoulliBanditGreedy [(action, SequentialMean)]


instance Eq action => BanditAgent (BernoulliBanditGreedy action) action Bool where
  selectAction (BernoulliBanditGreedy means) = selectActionGreedy means
  updateAgent action reward (BernoulliBanditGreedy means)
    = BernoulliBanditGreedy $ updateAgentGreedy action reward means


newBanditGreedy :: Eq action => [action] -> BernoulliBanditGreedy action
newBanditGreedy actions
  = if hasDuplicates actions
    then error "BernoulliBanditGreedy: actions need to be unique"
    else BernoulliBanditGreedy $ zip actions $ repeat newSequentialMean


-- TODO: would be interesting to examine the effect of unboxing the parameters
data SequentialMean = SequentialMean Int Double


instance SequentialEstimator SequentialMean Double Double where
  estimate (SequentialMean _ mean) = mean
  updateEstimate v (SequentialMean n mean)
    = let n' = n + 1
      in SequentialMean n' (mean + (v - mean) / fromIntegral n')


newSequentialMean :: SequentialMean
newSequentialMean = SequentialMean 0 0.0


-- TODO: break ties by uniform random sampling
selectActionGreedy :: Eq action
                   => [(action, SequentialMean)] -> RVar action
selectActionGreedy = pure . fst . foldl1 selectMax . fmap estimateMean
  where
    estimateMean (action, mean) = (action, estimate mean)


updateAgentGreedy :: Eq action
                  => action -> Bool -> [(action, SequentialMean)] -> [(action, SequentialMean)]
updateAgentGreedy action reward means = updateMeans action reward `fmap` means
  where updateMeans selected r (a, mean) =
          if selected == a
          then (selected, updateEstimate (if r then 1.0 else 0.0) mean)
          else (a, mean)

-- |Agent for a Bernoulli bandit problem that selects actions by Thompson sampling.
data BernoulliBanditTS action = BernoulliBanditTS [(action, D.Beta Double)]


instance Eq action => BanditAgent (BernoulliBanditTS action) action Bool where
  selectAction (BernoulliBanditTS prior)
    = selectActionTS prior
  updateAgent action reward (BernoulliBanditTS prior)
    = BernoulliBanditTS $ updateAgentTS action reward prior


selectMax :: Ord b => (a, b) -> (a, b) -> (a, b)
selectMax (selected, maxVariate) (action, variate) =
    if variate > maxVariate
    then (action, variate)
    else (selected, maxVariate)


selectActionTS :: (Ord v, Distribution D.Beta v)
              => [(action, D.Beta v)] -> RVar action
selectActionTS prior = do
  variates <- forM prior $ sampleMarginal
  pure $ fst $ foldl1 selectMax variates
  where sampleMarginal (action, marginal) = do
          variate <- rvar marginal
          pure (action, variate)

updateAgentTS :: Eq action
             => action -> Bool -> [(action, D.Beta Double)] ->  [(action, D.Beta Double)]
updateAgentTS action reward prior =
  updateMarginal action reward `fmap` prior
  where updateMarginal selected r (a, marginal@(D.Beta alpha beta)) =
          if selected == a
          then (selected, if r
                          then D.Beta (alpha + 1.0) beta
                          else D.Beta alpha (beta + 1.0))
          else (action, marginal)


-- |Creates a new Bernoulli Bandit from a conjugate prior. Fails with an error
-- if the actions in the prior have duplicates.
newBanditTS :: Eq action => [(action, D.Beta Double)] -> BernoulliBanditTS action
newBanditTS prior = if hasDuplicates (fst `fmap` prior)
                  then error "BernoulliBanditTS: actions need to be unique"
                  else BernoulliBanditTS prior


-- |Returns whether the list contains duplicates. Note that the complexity is
-- quadratic in the length of the list. This is due to the fact that we do not
-- want to impose an Ord constraint on the elements of the list.
hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates [] = False
hasDuplicates (x:xs) = x `elem` xs || hasDuplicates xs
