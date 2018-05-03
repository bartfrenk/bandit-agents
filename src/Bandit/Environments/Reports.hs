module Bandit.Environments.Reports where

import           Data.Foldable                          (toList)
import           Data.Sequence                          (Seq)
import qualified Data.Sequence                          as Seq
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy

import           Bandit.Environments.Types

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
    plot (line "" $ [zip [(0 :: Int)..] $ toList seq])
