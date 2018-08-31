module Bandit.Simulation where

import           Conduit
import           Control.Monad.Par
import           Control.Monad.State           (evalState)
import           Data.Function                 ((&))
import           Data.List                     (foldl')
import           Data.Random
import           Data.Random.Sample            (sample)
import           System.Random.Mersenne.Pure64 (PureMT)

import           Bandit.Agents.Types           (BanditAgent (..))
import           Bandit.Environments.Types     (Environment (..))

type Logger agent ctx act rew line = agent -> ctx -> act -> rew -> line

sourceSimulate ::
     (BanditAgent agent ctx act rew, Environment env ctx act rew, MonadRandom m)
  => Logger agent ctx act rew line
  -> env
  -> agent
  -> ConduitT () line m ()
sourceSimulate logger = loop
  where
    loop env agent = do
      ctx <- lift $ sample (generateContext env)
      act <- lift $ sample (selectAction agent ctx)
      case generateReward env ctx act of
        Just rvar -> do
          rew <- lift $ sample rvar
          yield $! logger agent ctx act rew
          loop env $! updateBelief ctx act rew agent
        Nothing -> loop env agent

simulate ::
     (BanditAgent agent ctx act rew, Environment env ctx act rew, MonadRandom m)
  => Logger agent ctx act rew line
  -> env
  -> agent
  -> m [line]
simulate logger = loop
  where
    loop env agent = do
      ctx <- sample (generateContext env)
      act <- sample (selectAction agent ctx)
      case generateReward env ctx act of
        Just rvar -> do
          rew <- sample rvar
          let !agent' = updateBelief ctx act rew agent
          (:) (logger agent ctx act rew) <$> loop env agent'
        Nothing -> loop env agent

-- |Logs the difference between the expected reward of an optimal action, and
-- the expected reward of the chosen action.
expectedRegretPerRound ::
     (BanditAgent agent ctx act rew, Environment env ctx act rew, Num rew)
  => env
  -> Logger agent ctx act rew rew
expectedRegretPerRound env _ ctx act _ =
  maximalReward env ctx - expectedReward env ctx act

simulatePure ::
     (BanditAgent agent ctx act rew, Environment env ctx act rew)
  => Logger agent ctx act rew line
  -> env
  -> PureMT
  -> agent
  -> [line]
simulatePure logger env gen agent = evalState (simulate logger env agent) gen

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (a:_) = Just a

colwiseAverage :: Fractional a => [[a]] -> [a]
colwiseAverage xss =
  let n = length xss
  in case sequence $ safeHead `fmap` xss of
       Nothing -> []
       Just xs -> avg n xs : (colwiseAverage $ tail `fmap` xss)
  where
    avg n xs = foldl' (+) (fromInteger 0) xs / (fromIntegral n)

type Summarizer line summary = [[line]] -> summary

simulateManyPure ::
     (BanditAgent agent ctx act rew, Environment env ctx act rew)
  => Logger agent ctx act rew line
  -> Summarizer line summary
  -> env
  -> [PureMT]
  -> agent
  -> summary
simulateManyPure logger summarizer env gens agent =
  (\gen -> simulatePure logger env gen agent) `fmap` gens & summarizer

simulateManyPar ::
     ( BanditAgent agent ctx act rew
     , Environment env ctx act rew
     , NFData line
     )
  => Logger agent ctx act rew line
  -> Summarizer line summary
  -> Int -- ^ The number of rounds in each simulation
  -> env
  -> [PureMT]
  -> agent
  -> summary
simulateManyPar logger summarizer rounds env gens agent =
  let logs =
        (\gen -> take rounds $ simulatePure logger env gen agent) `fmap` gens
  in runPar $ do
       ivars <- spawnP `mapM` logs
       summarizer <$> get `mapM` ivars
