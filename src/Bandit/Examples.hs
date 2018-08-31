{-# LANGUAGE FlexibleContexts #-}

module Bandit.Examples where

import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy

import           Data.Foldable                          (toList)
import           Numeric.LinearAlgebra                  (vector)

import           Bandit.Agents.Bernoulli
import           Bandit.Agents.Logistic                 (LogisticBanditTS)
import qualified Bandit.Agents.Logistic                 as L
import           Bandit.Environments.Bernoulli
import           Bandit.Environments.Logistic

import           Bandit.Simulation.Reports
import           Bandit.Simulation.Types

data Action
  = A
  | B
  | C
  deriving (Eq, Ord, Show)

logisticExperiment ::
     Monad m => Int -> ExperimentT Double m (LogisticBanditTS Action)
logisticExperiment trials =
  let acts = [A, B, C]
      agent = L.newBanditTS 3 acts
      env =
        newLogisticEnv $
        zip
          acts
          [ vector [1.0, 0.0, 0.0]
          , vector [0.0, 1.0, 0.0]
          , vector [0.0, 0.0, 1.0]
          ]
  in newExperiment agent env trials

bernoulliExperiment ::
     Monad m => Int -> ExperimentT Double m (BernoulliBanditTS Action)
bernoulliExperiment trials = do
  let actions = [A, B, C]
      agent = newBanditNI actions
      env = newBernoulliEnv $ zip actions [0.9, 0.8, 0.7]
  newExperiment agent env trials

testBernoulli :: Int -> IO (BernoulliBanditTS Action)
testBernoulli n = do
  let k = max (n `div` 100) 1
  (bandit, log) <- runExperiment (bernoulliExperiment n)
  toFile def ("bernoulli-" ++ show n ++ ".png") $ do
    layout_title .= "Average regret (" ++
      show n ++ " trials, " ++ show k ++ " trials per period)"
    plot (line "average regret" $ [zip [0 .. n] (toList $ averageRegret k log)])
  pure bandit

testLogistic :: Int -> IO (LogisticBanditTS Action)
testLogistic trials = do
  let k = max (trials `div` 100) 1
  (bandit, log) <- runExperiment (logisticExperiment trials)
  toFile def ("logistic-" ++ show trials ++ ".png") $ do
    layout_title .= "Average regret (" ++
      show trials ++ " trials, " ++ show k ++ " trials per period)"
    plot
      (line "average regret" $
       [zip [0 .. trials] (toList $ averageRegret k log)])
  pure bandit

plotLogisticAgent :: Int -> Int -> IO ()
plotLogisticAgent samples trials = do
  plotAveragePerPeriodRegret samples path (logisticExperiment trials)
  where
    path = "logistic-" ++ show samples ++ "-" ++ show trials ++ ".png"

plotBernoulliAgent :: Int -> Int -> IO ()
plotBernoulliAgent samples rounds = do
  plotAveragePerPeriodRegret samples path (bernoulliExperiment rounds)
  where
    path = "bernoulli-" ++ show samples ++ "-" ++ show rounds ++ ".png"

plotBernoulliAgentPar :: Int -> Int -> IO ()
plotBernoulliAgentPar samples rounds = do
  plotAveragePerPeriodRegretPar samples path (bernoulliExperiment rounds)
  where
    path = "bernoulli-" ++ show samples ++ "-" ++ show rounds ++ ".png"

