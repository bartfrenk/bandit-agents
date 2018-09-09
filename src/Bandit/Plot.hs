module Bandit.Plot
  ( sinkToLinePlot
  , plotLineToFile
  , plotLineMapToFile
  , PlotValue
  ) where

import           Conduit
import Control.Monad
import           Control.Monad.Trans                    (MonadIO, liftIO)
import           Data.Map.Strict                        (Map)
import qualified Data.Map.Strict                        as Map
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy


-- |Simple function to plot a list to a file.
plotLineToFile :: (MonadIO m, PlotValue a) => FilePath -> String -> [a] -> m ()
plotLineToFile path title xs =
  liftIO $
  toFile def path $ do
    layout_title .= title
    plot (line "" $ [zip [(0 :: Int) ..] $ xs])

plotLineMapToFile :: (MonadIO m, PlotValue a)
                  => FilePath -> String -> Map String [a] -> m ()
plotLineMapToFile path title m = liftIO $ toFile def path $ do
  layout_title .= title
  void $ traverse plotLine (Map.toList m)
  where
    plotLine (name, xs) = plot (line name $ [zip [(0 :: Int) ..] xs])


-- |Convenience function to plot `count` stream values to a file.
sinkToLinePlot ::
     (MonadIO m, PlotValue o)
  => Int
  -> FilePath
  -> String
  -> ConduitT () o m ()
  -> m ()
sinkToLinePlot count path title source = do
  xs <- runConduit $ source .| takeC count .| sinkList
  plotLineToFile path title xs
