module Bandit.Simulation.Reports where

import           Data.Foldable                          (toList)
import           Data.Sequence                          (Seq)
import qualified Data.Sequence                          as Seq
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy

import           Bandit.Simulation.Types

averagePerPeriodRegret :: (Num rew, Fractional rew) => [Log rew] -> Seq rew
averagePerPeriodRegret [] = error "no logs"
averagePerPeriodRegret logs = (/ (fromIntegral (length logs))) <$> total
  where
    total = loop (tail logs) $ perPeriodRegret (head logs)
    loop [] acc = acc
    loop (log:remainder) acc =
      let regret = perPeriodRegret log
      in loop remainder $ Seq.zipWith (+) acc regret

plotSequence :: (PlotValue rew, Num rew) => String -> String -> Seq rew -> IO ()
plotSequence path title seq = do
  toFile def path $ do
    layout_title .= title
    plot (line "" $ [zip [(0 :: Int) ..] $ toList seq])

plotAveragePerPeriodRegret ::
     (PlotValue rew, Num rew, Fractional rew)
  => Int
  -> String
  -> ExperimentT rew IO agent
  -> IO ()
plotAveragePerPeriodRegret samples path experiment = do
  logs <- runMany samples experiment
  plotSequence path title $ averagePerPeriodRegret logs
  where
    title = "Average per period regret (" ++ show samples ++ " samples)"

cumulativeRegret :: Num rew => Log rew -> Seq rew
cumulativeRegret = Seq.scanl (+) 0 . perPeriodRegret

averageRegret :: (Fractional rew, Num rew) => Int -> Log rew -> Seq rew
averageRegret k log =
  let xs = cumulativeRegret log
      (h, t) = Seq.splitAt k xs
      totalRegret = Seq.scanl (+) (sum h) $ Seq.zipWith (-) t xs
  in (/ (fromIntegral k)) <$> totalRegret

perPeriodRegret :: Num rew => Log rew -> Seq rew
perPeriodRegret (Log seq) = uncurry (-) `fmap` seq

