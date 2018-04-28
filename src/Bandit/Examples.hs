module Bandit.Examples where

import Data.Random
import Data.Random.Source.DevRandom (DevRandom(..))
import Data.Random.Source.IO
import Data.Random.Sample


import Bandit.Bernoulli
import Bandit.Environments.Bernoulli
import Bandit.Environments.Types


data Action
  = A | B | C deriving (Eq, Ord, Show)

bernoulliExperiment :: Int -> RVarT IO (BernoulliBanditTS Action)
bernoulliExperiment trials = do
  let actions = [A, B, C]
      agent = newBanditNI actions
      env = newBernoulliEnv $ zip actions [0.6, 0.5, 0.5]
  newExperiment agent env trials

test :: IO (BernoulliBanditTS Action)
test = do
  sample (bernoulliExperiment 10000)
