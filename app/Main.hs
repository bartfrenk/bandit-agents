{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Monad
import           Control.Monad.Par                    (NFData)
import           Data.Aeson
import           System.Environment                   (getArgs)
import           System.Random.Mersenne.Pure64

import           Bandit.Agents.Bernoulli
import           Bandit.Environments.Bernoulli        (newBernoulliEnv)
import           Bandit.Environments.Types
import           Bandit.Experimentation.Serialization
import           Bandit.Plot
import           Bandit.Simulation

import Debug.Trace

-- runWithBandit ::
--      (MonadIO m, Environment env () String Double) --   => env
--   -> Int
--   -> Int
--   -> WithMeta Value
--   -> m [Double]
-- runWithBandit env samples rounds WithMeta {_meta, _data} =
--   let logger = expectedRegretPerRound env
--       summarizer = colwiseAverage
--   in case schema _meta of
--        "urn:bandit:schema:agent:bernoulli:thompson-sampling:v1" -> do
--          gens <- liftIO $ replicateM samples newPureMT
--          let result = fromJSON _data :: Result (BernoulliBanditTS String)
--          case result of
--            Error _ -> pure []
--            Success agent -> do
--              pure $ simulateManyPar logger summarizer rounds env gens agent
--        _ -> pure []

-- runWithBandit' ::
--      (Environment env () String Double)
--   => env
--   -> WithMeta Value
--   -> (forall agent. BanditAgent agent () String Double =>
--                       env -> agent -> [Double])
--   -> Maybe [Double]
-- runWithBandit' env WithMeta {_meta, _data} run =
--   case schema _meta of
--     "urn:bandit:schema:agent:bernoulli:thompson-sampling:v1" -> do
--       let result = fromJSON _data :: Result (BernoulliBanditTS String)
--       case result of
--         Error _ -> Nothing
--         Success agent -> do
--           Just $ run env agent
--     _ -> Nothing

-- runWithBandit'' :: WithMeta Value -> WithBandit String Double -> Maybe [Double]
-- runWithBandit'' WithMeta {_meta, _data} run =
--   case schema _meta of
--     "urn:bandit:schema:agent:bernoulli:thompson-sampling:v1" -> do
--       let result = fromJSON _data :: Result (BernoulliBanditTS String)
--       case result of
--         Error _ -> Nothing
--         Success agent -> Just $ run agent
--     _ -> Nothing

-- --test :: WithMeta Value -> IO (Maybe [Double])
-- test :: Int -> [PureMT] -> WithBandit String Double
-- test rounds gens =
--   let env = newBernoulliEnv [("x", 0.01), ("y", 0.02), ("z", 0.03)]
--       logger = expectedRegretPerRound env
--       summarizer = colwiseAverage
--   in \agent -> simulateManyPar logger summarizer rounds env gens agent

-- test' :: WithMeta Value -> IO (Maybe [Double])
-- test' withMeta = do
--   [samples, rounds] <- getArgs
--   gens <- replicateM (read samples) newPureMT
--   pure $ runWithBandit'' withMeta (test (read rounds) gens)

-- simulateInEnv ::
--      (NFData line, Environment env ctx act rew)
--   => GenericLogger ctx act rew line
--   -> Summarizer line summary
--   -> Int
--   -> env
--   -> [PureMT]
--   -> RunAgent ctx act rew summary
-- simulateInEnv = simulateManyPar

runWithAgent :: WithMeta Value -> RunAgent () String Double summary -> Maybe summary
runWithAgent WithMeta {_meta, _data} run =
  case schema _meta of
    "urn:bandit:schema:agent:bernoulli:thompson-sampling:v1" ->
      let result = fromJSON _data :: Result (BernoulliBanditTS String)
      in case result of
        Error _ -> Nothing
        Success agent -> trace (show agent) Just $ run agent
    _ -> Nothing

main :: IO ()
main = do
  [samples, rounds] <- getArgs
  decodeFileStrict' "res/bernoulli-ts.json" >>= \case
    Nothing -> error "Not a valid file"
    Just withMeta -> do
      gens <- replicateM (read samples) newPureMT
      let env = newBernoulliEnv [("A", 0.1), ("B", 0.2), ("C", 0.8)]
      trace (show env) $ case runWithAgent withMeta (metric env gens (read rounds)) of
        Nothing -> pure ()
        Just log -> plotLineToFile "res/bernoulli.png" "<title>" log

  where
    metric env gens rounds =
      let summarizer = colwiseAverage
      in simulateManyPar (expectedRegretPerRound env) summarizer rounds env gens



-- metric :: Environment env ctx String Double
--        => env -> [PureMT] -> Int -> RunAgent ctx String Double [Double]


-- main :: IO ()
-- main = do
--   [samples, rounds] <- getArgs
--   let agent = newBanditNI ["A", "B", "C"]
--   let env = newBernoulliEnv [("A", 0.1), ("B", 0.2), ("C", 0.8)]
--   let logger = expectedRegretPerRound env
--   let summarizer = colwiseAverage
--   gens <- replicateM (read samples) newPureMT
--   let log = simulateManyPar logger summarizer (read rounds) env gens agent
--   plotLineToFile "docs/_img/bernoulli.png" "<title>" log
