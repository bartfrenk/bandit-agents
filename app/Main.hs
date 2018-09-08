{-# LANGUAGE RankNTypes #-}
import           Control.Monad
import           Data.Aeson                    as JSON
import           System.Environment            (getArgs)
import           System.Random.Mersenne.Pure64

import           Bandit.Agents.Bernoulli       as Bernoulli
import           Bandit.Agents.Serialization
import           Bandit.Environments.Bernoulli (newBernoulliEnv)
import           Bandit.Plot
import           Bandit.Simulation
import           Data.Yaml                     as YAML
import           Options.Applicative


runWithAgent :: WithMeta Value -> RunAgent () String Double summary -> Maybe summary
runWithAgent WithMeta {_meta, _data} run =
  case schema _meta of
    "urn:bandit:schema:agent:bernoulli:thompson:v1" ->
      let result = fromJSON _data :: Result (Bernoulli.ThompsonAgent String)
      in case result of
        JSON.Error _ -> Nothing
        JSON.Success agent -> Just $ run agent
    _ -> Nothing

runFromFile :: FilePath -> RunAgent () String Double summary -> IO (Maybe summary)
runFromFile path runAgent = do
  YAML.decodeFileEither path >>= \case
    Left err -> error (show err)
    Right withMeta -> pure $ runWithAgent withMeta runAgent

main :: IO ()
main = do
  [samples, rounds] <- getArgs
  decodeFileStrict' "res/bernoulli-thompson.json" >>= \case
    Nothing -> error "Not a valid file"
    Just withMeta -> do
      gens <- replicateM (read samples) newPureMT
      case runWithAgent withMeta (metric gens (read rounds)) of
        Nothing -> pure ()
        Just log -> plotLineToFile "res/bernoulli.png" "<title>" log

  where
    env = newBernoulliEnv [("A", 0.1), ("B", 0.2), ("C", 0.8)]
    metric gens rounds =
      let summarizer = colwiseAverage
      in simulateManyPar (expectedRegretPerRound env) summarizer rounds env gens
