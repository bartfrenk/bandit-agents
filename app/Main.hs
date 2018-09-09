{-# LANGUAGE RankNTypes #-}
import           Control.Monad
import           Data.Aeson                    as JSON
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as Map
import           System.Environment            (getArgs)
import           System.Random.Mersenne.Pure64

import           Bandit.Agents.Bernoulli       as Bernoulli
import           Bandit.Agents.Serialization
import           Bandit.Environments.Bernoulli (newBernoulliEnv)
import           Bandit.Plot
import           Bandit.Simulation
import           Data.Yaml                     as YAML


runWithAgent :: WithMeta Value
             -> RunAgent () String Double summary
             -> Either String summary
runWithAgent WithMeta {_meta, _data} run =
  case schema _meta of
    "urn:bandit:schema:agent:bernoulli:thompson:v1" ->
      let result = fromJSON _data :: Result (Bernoulli.ThompsonAgent String)
      in case result of
        JSON.Error err -> Left $ show err
        JSON.Success agent -> Right $ run agent
    "urn:bandit:schema:agent:bernoulli:greedy:v1" ->
      let result = fromJSON _data :: Result (Bernoulli.GreedyAgent String)
      in case result of
        JSON.Error err -> Left $ show err
        JSON.Success agent -> Right $ run agent

    urn -> Left $ "Unknown schema " ++ urn

runFromFile :: FilePath
            -> RunAgent () String Double summary
            -> IO (Either String summary)
runFromFile path runAgent = do
  YAML.decodeFileEither path >>= \case
    Left err -> error (show err)
    Right withMeta -> pure $ runWithAgent withMeta runAgent

runAgentsFromFile :: FilePath
                  -> RunAgent () String Double summary
                  -> IO (Map String (Either String summary))
runAgentsFromFile path runAgent =
  YAML.decodeFileEither path >>= \case
    Left err -> error (show err)
    Right experiment ->
      let fn withMeta = (label $ _meta withMeta, runWithAgent withMeta runAgent)
      in pure $ Map.fromList $ (fn <$> agents experiment)

main :: IO ()
main = do
  [samples, rounds] <- getArgs
  gens <- replicateM (read samples) newPureMT
  lineMap <- runAgentsFromFile path (doSimulation gens $ read rounds)
  case sequenceA lineMap of
    Left err -> error err
    Right m -> plotLineMapToFile "res/bernoulli-experiment.png" path m
  where
    path = "res/bernoulli-experiment.yaml"
    env = newBernoulliEnv [("A", 0.1), ("B", 0.2), ("C", 0.8)]
    doSimulation gens rounds =
      let summarizer = colwiseAverage
      in simulateManyPar (expectedRegretPerRound env) summarizer rounds env gens
