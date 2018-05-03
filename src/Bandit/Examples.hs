{-# LANGUAGE FlexibleContexts #-}
module Bandit.Examples where

import Control.Arrow (second)
import Control.Monad
import Data.Sequence (Seq)
import Control.Monad.Writer
import Data.Random
import Data.Random.Source.DevRandom (DevRandom(..))
import Data.Random.Source.IO
import Data.Random.Sample
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import qualified Data.Sequence as Seq
import Data.Foldable (toList)

import Bandit.Bernoulli
import Bandit.Environments.Bernoulli
import Bandit.Environments.Types
import Bandit.Experiments


data Action
  = A | B | C deriving (Eq, Ord, Show)




bernoulliExperiment :: Monad m => Int -> ExperimentT Double m (BernoulliBanditTS Action)
bernoulliExperiment trials = do
  let actions = [A, B, C]
      agent = newBanditNI actions
      env = newBernoulliEnv $ zip actions [0.9, 0.8, 0.7]
  newExperiment agent env trials

test :: Int -> IO (BernoulliBanditTS Action)
test n = do
  let k = max (n `div` 100) 1
  (bandit, log) <- runExperiment (bernoulliExperiment n)
  toFile def ("bernoulli-" ++ show n ++ ".png") $ do
    layout_title .= "Average regret (" ++ show n ++ " trials, "
      ++ show k ++ " trials per period)"
    plot (line "average regret" $ [zip [0..n] (toList $ averageRegret k log)])
  pure bandit


plotBernoulliAgent :: Int -> Int -> IO ()
plotBernoulliAgent samples trials = do
  plotAveragePerPeriodRegret samples path (bernoulliExperiment trials)
  where
    path = "bernoulli-" ++ show samples ++ "-" ++ show trials ++ ".png"
