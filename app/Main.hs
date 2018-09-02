{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Control.Monad.Par (NFData)
import Control.Monad.Trans
import System.Environment (getArgs)
import System.Random.Mersenne.Pure64

import Bandit.Agents.Bernoulli
import Bandit.Agents.Types
import Bandit.Environments.Bernoulli (newBernoulliEnv)
import Bandit.Environments.Types
import Bandit.Experimentation.Serialization
import Bandit.Plot
import Bandit.Simulation
import Data.Aeson

data Action
  = A
  | B
  | C
  deriving (Eq, Ord)

actions :: [Action]
actions = [A, B, C]

runWithBandit ::
     (MonadIO m, Environment env () String Double)
  => env
  -> Int
  -> Int
  -> WithMeta Value
  -> m [Double]
runWithBandit env samples rounds WithMeta {_meta, _data} =
  let logger = expectedRegretPerRound env
      summarizer = colwiseAverage
  in case schema _meta of
       "urn:bandit:schema:agent:bernoulli:thompson-sampling:v1" -> do
         gens <- liftIO $ replicateM samples newPureMT
         let result = fromJSON _data :: Result (BernoulliBanditTS String)
         case result of
           Error _ -> pure []
           Success agent -> do
             pure $ simulateManyPar logger summarizer rounds env gens agent
       _ -> pure []

runWithBandit' ::
     (Environment env () String Double)
  => env
  -> WithMeta Value
  -> (forall agent. BanditAgent agent () String Double =>
                      env -> agent -> [Double])
  -> Maybe [Double]
runWithBandit' env WithMeta {_meta, _data} run =
  case schema _meta of
    "urn:bandit:schema:agent:bernoulli:thompson-sampling:v1" -> do
      let result = fromJSON _data :: Result (BernoulliBanditTS String)
      case result of
        Error _ -> Nothing
        Success agent -> do
          Just $ run env agent
    _ -> Nothing

runWithBandit'' :: WithMeta Value -> WithBandit String Double -> Maybe [Double]
runWithBandit'' WithMeta {_meta, _data} run =
  case schema _meta of
    "urn:bandit:schema:agent:bernoulli:thompson-sampling:v1" -> do
      let result = fromJSON _data :: Result (BernoulliBanditTS String)
      case result of
        Error _ -> Nothing
        Success agent -> do
          Just $ run agent
    _ -> Nothing

--test :: WithMeta Value -> IO (Maybe [Double])
test :: Int -> [PureMT] -> WithBandit String Double
test rounds gens =
  let env = newBernoulliEnv [("x", 0.01), ("y", 0.02), ("z", 0.03)]
      logger = expectedRegretPerRound env
      summarizer = colwiseAverage
  in \agent -> simulateManyPar logger summarizer rounds env gens agent

test' :: WithMeta Value -> IO (Maybe [Double])
test' withMeta = do
  [samples, rounds] <- getArgs
  gens <- replicateM (read samples) newPureMT
  pure $ runWithBandit'' withMeta (test (read rounds) gens)

-- |Function to run on a generic agent.
type RunAgent ctx act rew summary
   = (forall agent. BanditAgent agent ctx act rew =>
                      agent -> summary)

-- |Logger that does not depend on the agent
type GenericLogger ctx act rew line
   = (forall agent. Logger agent ctx act rew line)

simulateInEnv ::
     (NFData line, Environment env ctx act rew)
  => GenericLogger ctx act rew line
  -> Summarizer line summary
  -> Int
  -> env
  -> [PureMT]
  -> RunAgent ctx act rew summary
simulateInEnv = simulateManyPar


runWithAgent :: WithMeta Value -> RunAgent () String Double summary -> Maybe summary
runWithAgent WithMeta {_meta, _data} run =
  case schema _meta of
    "urn:bandit:schema:agent:bernoulli:thompson-sampling:v1" -> do
      let result = fromJSON _data :: Result (BernoulliBanditTS String)
      case result of
        Error _ -> Nothing
        Success agent -> do
          Just $ run agent
    _ -> Nothing

type WithBandit act rew
   = (forall agent. BanditAgent agent () act rew =>
                      agent -> [rew])

-- bla withMeta = do
--   x <- test
--   pure $ runWithBandit'' withMeta x
main :: IO ()
main = do
  [samples, rounds] <- getArgs
  let agent = newBanditNI actions
  let env = newBernoulliEnv [(A, 0.01), (B, 0.02), (C, 0.03)]
  let logger = expectedRegretPerRound env
  let summarizer = colwiseAverage
  gens <- replicateM (read samples) newPureMT
  let log = simulateManyPar logger summarizer (read rounds) env gens agent
  plotLineToFile "docs/_img/bernoulli.png" "<title>" log
