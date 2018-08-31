{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Estimators where

class SequentialEstimator estimator est obs | estimator -> est obs where
  estimate :: estimator -> est
  updateEstimate :: obs -> estimator -> estimator

data SequentialMean =
  SequentialMean Int
                 Double
  deriving (Show)

instance SequentialEstimator SequentialMean Double Double where
  estimate (SequentialMean _ mean) = mean
  updateEstimate v (SequentialMean n mean) =
    let n' = n + 1
    in SequentialMean n' (mean + (v - mean) / fromIntegral n')

newSequentialMean :: SequentialMean
newSequentialMean = SequentialMean 0 0.0
