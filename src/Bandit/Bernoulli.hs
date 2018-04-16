{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Bandit.Bernoulli
  (newSequentialMean,
   newBanditTS,
   newBanditGreedy,
   BernoulliBanditGreedy,
   BernoulliBanditTS) where


import Control.Monad
import Data.Random
import qualified Data.Random.Distribution.Beta as D

import Bandit.Types

data BernoulliBanditRandom action = BernoulliBanditRandom [action]


instance Eq action => BanditAgent (BernoulliBanditRandom action) where
  type Reward (BernoulliBanditRandom action) = Bool
  type Action (BernoulliBanditRandom action) = action
  selectAction (BernoulliBanditRandom actions) = randomElement actions
  updateAgent _ _ agent = agent


data BernoulliBanditGreedy action = BernoulliBanditGreedy [(action, SequentialMean)]


instance Eq action => BanditAgent (BernoulliBanditGreedy action) where
  type Reward (BernoulliBanditGreedy action) = Bool
  type Action (BernoulliBanditGreedy action) = action
  selectAction (BernoulliBanditGreedy means) = selectActionGreedy means
  updateAgent action reward (BernoulliBanditGreedy means)
    = BernoulliBanditGreedy $ updateAgentGreedy action reward means


newBanditGreedy :: Eq action => [action] -> BernoulliBanditGreedy action
newBanditGreedy actions
  = if hasDuplicates actions
    then error "BernoulliBanditGreedy: actions need to be unique"
    else BernoulliBanditGreedy $ zip actions $ repeat newSequentialMean

data SequentialMean = SequentialMean Int Double


instance SequentialEstimator SequentialMean where
  type Estimate SequentialMean = Double
  type Observation SequentialMean = Double
  estimate (SequentialMean _ mu) = mu
  updateEstimate v (SequentialMean n mu)
    = let n' = n + 1
      in SequentialMean n' (mu + (v - mu) / fromIntegral n')


newSequentialMean :: SequentialMean
newSequentialMean = SequentialMean 0 0.0

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


instance Eq action => BanditAgent (BernoulliBanditTS action) where
  type Reward (BernoulliBanditTS action) = Bool
  type Action (BernoulliBanditTS action) = action
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
