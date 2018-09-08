module Bandit.Agents.Bernoulli
  ( newGreedyAgent
  , newThompsonAgent
  , newThompsonAgentNI
  , newRandomAgent
  , GreedyAgent
  , RandomAgent
  , ThompsonAgent
  ) where

import           Control.Monad
import           Data.Random
import qualified Data.Random.Distribution.Beta as D
import qualified Data.Vector                   as V
import           Data.Yaml

import           Bandit.Agents.Types
import           Bandit.Utils                  (selectMax)
import           Estimators


-- |Creates a new agent that selects an action uniformly at random.
newRandomAgent :: Eq action => [action] -> RandomAgent action
newRandomAgent actions =
  if hasDuplicates actions
    then error "RandomAgent: actions need to be unique"
    else RandomAgent actions

-- |Creates a new agent that keeps a point estimate of the expected reward, and
-- selects the action with the highest point estimate.
newGreedyAgent :: Eq action => [action] -> GreedyAgent action
newGreedyAgent actions =
  if hasDuplicates actions
    then error "GreedyAgent: actions need to be unique"
    else GreedyAgent $ zip actions $ repeat newSequentialMean

-- |Creates an agent that selects an action by Thompson sampling. The prior
-- belief about the expected reward of each action is encoded in the `prior`
-- parameter.
newThompsonAgent ::
     Eq action => [(action, D.Beta Double)] -> ThompsonAgent action
newThompsonAgent prior =
  if hasDuplicates (fst `fmap` prior)
    then error "ThompsonAgent: actions need to be unique"
    else ThompsonAgent prior

-- |Like `newBanditTS`, but starting with a uniform (non-informative) prior.
newThompsonAgentNI :: Eq action => [action] -> ThompsonAgent action
newThompsonAgentNI acts = newThompsonAgent $ zip acts (repeat $ D.Beta 1.0 1.0)

data RandomAgent action =
  RandomAgent ![action]

instance Eq act => BanditAgent (RandomAgent act) () act Double where
  selectAction (RandomAgent acts) _ctx = randomElement acts
  updateBelief _ctx _act _rew agent = agent

data GreedyAgent action =
  GreedyAgent ![(action, SequentialMean)]
  deriving (Show)

instance Eq act => BanditAgent (GreedyAgent act) () act Double where
  selectAction (GreedyAgent means) _ctx = selectActionGreedy means
  updateBelief _ctx act rew (GreedyAgent means) =
    GreedyAgent $ updateAgentGreedy act rew means

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
data ThompsonAgent action =
  ThompsonAgent ![(action, D.Beta Double)]

instance FromJSON act => FromJSON (ThompsonAgent act) where
  parseJSON = withArray "ThompsonAgent" $ \arr ->
    ThompsonAgent . V.toList <$> forM arr parseAction

    where parseAction = withObject "Action" $ \obj -> (,)
            <$> obj .: "action"
            <*> (obj .: "prior" >>= parseBeta)
          parseBeta = withObject "Beta" $ \obj -> D.Beta
            <$> obj .: "alpha"
            <*> obj .: "beta"

instance Show act => Show (ThompsonAgent act) where
  show (ThompsonAgent prior) = show $ f `fmap` prior
    where
      f (action, (D.Beta a b)) = (action, (a, b))

instance Eq act => BanditAgent (ThompsonAgent act) () act Double where
  selectAction (ThompsonAgent prior) _ctx = selectActionTS prior
  updateBelief _ctx act rew (ThompsonAgent prior) =
    ThompsonAgent $ updateAgentTS act rew prior

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

-- |Returns whether the list contains duplicates. Note that the complexity is
-- quadratic in the length of the list. This is due to the fact that we do not
-- want to impose an Ord constraint on the elements of the list.
hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates [] = False
hasDuplicates (x:xs) = x `elem` xs || hasDuplicates xs
