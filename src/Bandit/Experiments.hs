module Bandit.Experiments where

import Control.Monad.Trans (MonadIO, liftIO)
import           Graphics.Rendering.Chart.Easy

import Bandit.Environments.Types
import Bandit.Environments.Reports


plotAveragePerPeriodRegret :: (PlotValue rew, Num rew, Fractional rew)
                           => Int -> String -> ExperimentT rew IO agent -> IO ()
plotAveragePerPeriodRegret samples path experiment = do
  logs <- runMany samples experiment
  plotSequence path title $ averagePerPeriodRegret logs
  where title = "Average per period regret (" ++ show samples ++ " samples)"
