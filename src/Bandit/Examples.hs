module Bandit.Examples where


import Data.Random
import Data.Random.Source.DevRandom

import Bandit.Types
import Bandit.Bernoulli


-- |Selecting an action:
selectDevRandom :: BanditAgent agent action reward => agent -> IO action
selectDevRandom agent = runRVar (selectAction $ agent) DevRandom


data Action = A | B | C deriving (Eq, Show)

selectEpsilonGreedy :: IO Action
selectEpsilonGreedy = selectDevRandom (newBanditEpsilonGreedy 0.9 [A, B, C])
