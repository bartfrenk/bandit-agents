import           Control.Monad
import           System.Environment             (getArgs)
import           System.Random.Mersenne.Pure64

import           Bandit.Agents.Bernoulli        (newBanditNI)
import           Bandit.Environments.Bernoulli  (newBernoulliEnv)
import           Bandit.Plot
import           Bandit.Simulation

data Action
  = A
  | B
  | C
  deriving (Eq, Ord)

actions :: [Action]
actions = [A, B, C]

-- main :: IO ()
-- main = do
--   [rounds] <- getArgs
--   let agent = newBanditNI actions
--   let env = newBernoulliEnv [(A, 0.1), (B, 0.9), (C, 0.1)]
--   let logger = expectedRegretPerRound env
--   plotSimulation logger env agent (read rounds) "plots/bernoulli.png" "<title>"
-- main :: IO ()
-- main = do
--   [rounds] <- getArgs
--   let agent = newBanditNI actions
--   let env = newBernoulliEnv [(A, 0.1), (B, 0.9), (C, 0.1)]
--   let logger = expectedRegretPerRound env
--   gen <- newStdGen
--   let log = take (read rounds) $  simulatePure logger env gen agent
--   plotLineToFile "plots/bernoulli.png" "<title>" log
-- main :: IO ()
-- main = do
--   [samples, rounds] <- getArgs
--   let agent = newBanditNI actions
--   let env = newBernoulliEnv [(A, 0.1), (B, 0.9), (C, 0.1)]
--   let logger = expectedRegretPerRound env
--   let summarizer = colwiseAverage
--   gens <- replicateM (read samples) newStdGen
--   let log = simulateManyPure logger summarizer env gens agent
--   plotLineToFile "plots/bernoulli.png" "<title>" (take (read rounds) log)

main :: IO ()
main = do
  [samples, rounds] <- getArgs
  let agent = newBanditNI actions
  let env = newBernoulliEnv [(A, 0.01), (B, 0.02), (C, 0.03)]
  let logger = expectedRegretPerRound env
  let summarizer = colwiseAverage
  gens <- replicateM (read samples) newPureMT
  let log = simulateManyPar logger summarizer (read rounds) env gens agent
  plotLineToFile "plots/bernoulli.png" "<title>" log
