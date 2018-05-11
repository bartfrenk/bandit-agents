{-# LANGUAGE BangPatterns #-}

import           Criterion.Main

import           Control.Monad
import           Data.Random
import           Data.Random.Distribution.Bernoulli
import           Numeric.LinearAlgebra
import           System.Random

import qualified Bandit.Agents.Bernoulli            as B
import qualified Bandit.Agents.Logistic             as L
import           Bandit.Agents.Types
import qualified Bandit.Environments.Logistic       as L
import           Bandit.Utils                       (sigmoid)

main :: IO ()
main = defaultMain [benchLogistic, benchSequentialMean]

benchSequentialMean :: Benchmark
benchSequentialMean =
  let estimator = B.newSequentialMean
  in bgroup
       "sequential estimate of the mean"
       [benchmarkUpdateEstimate "single observation" 10.0 estimator]

benchLogistic :: Benchmark
benchLogistic =
  bgroup
    "logistic agent"
    [ let (n, k) = (1000, 10)
          batch = fst $ sampleState (randomBatch (k |> (repeat 0.0)) n) rng
          agent = L.newBanditTS k [()]
      in benchmarkBatchUpdateBelief
           "update with 1000 obs of dim 10"
           ((), batch)
           agent
    , let (n, k) = (10, 10)
          batch = fst $ sampleState (randomBatch (k |> (repeat 0.0)) n) rng
          agent = L.newBanditTS k [()]
      in benchmarkBatchUpdateBelief
           "update with 10 obs of dim 10"
           ((), batch)
           agent
    ]
  where
    rng = mkStdGen 0

randomBatch :: Vector Double -> Int -> RVar L.Batch
randomBatch w batchSize = do
  contexts <- replicateM batchSize $ L.randomContext (size w)
  let ps = sigmoid . (w <.>) <$> contexts
  targets <- mapM sample (bernoulli <$> ps)
  pure $ L.Batch {L.ctxs = fromRows contexts, L.targets = vector targets}

benchmarkBatchUpdateBelief ::
     (BatchUpdateBelief agent batch) => String -> batch -> agent -> Benchmark
benchmarkBatchUpdateBelief descr !batch !agent =
  bench descr $ whnf (batchUpdateBelief batch) agent

benchmarkUpdateEstimate ::
     (SequentialEstimator estimator est obs)
  => String
  -> obs
  -> estimator
  -> Benchmark
benchmarkUpdateEstimate descr !obs !estimator =
  bench descr $ whnf (updateEstimate obs) estimator
