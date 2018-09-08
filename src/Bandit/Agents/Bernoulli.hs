module Bandit.Agents.Bernoulli
  ( newBanditTS
  , newBanditNI
  , newBanditGreedy
  , newBanditRandom
  , newBanditEpsilonGreedy
  , BernoulliBanditGreedy
  , BernoulliBanditRandom
  , BernoulliBanditTS
  ) where

import Control.Monad
import Data.Random
import qualified Data.Random.Distribution.Beta as D
import Data.Yaml
import qualified Data.Vector as V

import Bandit.Agents.Combinators
import Bandit.Agents.Types
import Bandit.Utils (selectMax)
import Estimators

data BernoulliBanditRandom action =
  BernoulliBanditRandom ![action]

newBanditEpsilonGreedy :: Eq act => Double -> [act] -> MixedBandit () act Double
newBanditEpsilonGreedy p actions =
  newMixedAgent p (newBanditRandom actions) (newBanditGreedy actions)

newBanditRandom :: Eq action => [action] -> BernoulliBanditRandom action
newBanditRandom actions =
  if hasDuplicates actions
    then error "BernoulliBanditRandom: actions need to be unique"
    else BernoulliBanditRandom actions

instance Eq act => BanditAgent (BernoulliBanditRandom act) () act Double where
  selectAction (BernoulliBanditRandom acts) _ctx = randomElement acts
  updateBelief _ctx _act _rew agent = agent

data BernoulliBanditGreedy action =
  BernoulliBanditGreedy ![(action, SequentialMean)]
  deriving (Show)

instance Eq act => BanditAgent (BernoulliBanditGreedy act) () act Double where
  selectAction (BernoulliBanditGreedy means) _ctx = selectActionGreedy means
  updateBelief _ctx act rew (BernoulliBanditGreedy means) =
    BernoulliBanditGreedy $ updateAgentGreedy act rew means

newBanditGreedy :: Eq action => [action] -> BernoulliBanditGreedy action
newBanditGreedy actions =
  if hasDuplicates actions
    then error "BernoulliBanditGreedy: actions need to be unique"
    else BernoulliBanditGreedy $ zip actions $ repeat newSequentialMean

selectActionGreedy :: Eq action => [(action, SequentialMean)] -> RVar action
selectActionGreedy = selectMax . fmap estimateMean
  where
    estimateMean (action, mean) = (action, estimate mean)

updateAgentGreedy ::
     Eq action
  => action
  -> Double
  -> [(action, SequentialMean)]
  -> [(action, SequentialMean)]
updateAgentGreedy action reward means = updateMeans action reward `fmap` means
  where
    updateMeans selected r (a, mean) =
      if selected == a
        then (selected, updateEstimate r mean)
        else (a, mean)

-- |Agent for a Bernoulli bandit problem that selects actions by Thompson sampling.
data BernoulliBanditTS action =
  BernoulliBanditTS ![(action, D.Beta Double)]

instance FromJSON act => FromJSON (BernoulliBanditTS act) where
  parseJSON = withArray "BernoulliBanditTS" $ \arr ->
    BernoulliBanditTS . V.toList <$> forM arr parseAction

    where parseAction = withObject "Action" $ \obj -> (,)
            <$> obj .: "action"
            <*> (obj .: "prior" >>= parseBeta)
          parseBeta = withObject "Beta" $ \obj -> D.Beta
            <$> obj .: "alpha"
            <*> obj .: "beta"

instance Show act => Show (BernoulliBanditTS act) where
  show (BernoulliBanditTS prior) = show $ f `fmap` prior
    where
      f (action, (D.Beta a b)) = (action, (a, b))

instance Eq act => BanditAgent (BernoulliBanditTS act) () act Double where
  selectAction (BernoulliBanditTS prior) _ctx = selectActionTS prior
  updateBelief _ctx act rew (BernoulliBanditTS prior) =
    BernoulliBanditTS $ updateAgentTS act rew prior

selectActionTS ::
     (Ord v, Distribution D.Beta v) => [(action, D.Beta v)] -> RVar action
selectActionTS prior = do
  scores <- forM prior sampleMarginal
  selectMax scores
  where
    sampleMarginal (action, marginal) = do
      variate <- rvar marginal
      pure (action, variate)

updateAgentTS ::
     Eq action
  => action
  -> Double
  -> [(action, D.Beta Double)]
  -> [(action, D.Beta Double)]
updateAgentTS action reward prior = updateMarginal action reward `fmap` prior
  where
    updateMarginal selected r (a, marginal@(D.Beta alpha beta)) =
      if selected == a
        then ( selected
             , if r == 1.0
                  -- Work around the non-strictness of the D.Beta data constructor
                 then let !alpha' = alpha + 1.0
                      in D.Beta alpha' beta
                 else let !beta' = beta + 1.0
                      in D.Beta alpha beta')
        else (a, marginal)

-- |Creates a new Bernoulli Bandit from a conjugate prior. Fails with an error
-- if the actions in the prior have duplicates.
newBanditTS ::
     Eq action => [(action, D.Beta Double)] -> BernoulliBanditTS action
newBanditTS prior =
  if hasDuplicates (fst `fmap` prior)
    then error "BernoulliBanditTS: actions need to be unique"
    else BernoulliBanditTS prior

-- |Creates a new Bernoulli Bandit from a set of actions. The priors are
-- non-informative Beta distributions.
newBanditNI :: Eq action => [action] -> BernoulliBanditTS action
newBanditNI acts = newBanditTS $ zip acts (repeat $ D.Beta 1.0 1.0)

-- |Returns whether the list contains duplicates. Note that the complexity is
-- quadratic in the length of the list. This is due to the fact that we do not
-- want to impose an Ord constraint on the elements of the list.
hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates [] = False
hasDuplicates (x:xs) = x `elem` xs || hasDuplicates xs
