module Bandit.Utils
  ( selectMax
  , selectMaxMap
  , sigmoid
  , keepMax
  ) where

import Data.Map.Strict (Map)
import Data.Random.List
import qualified Data.Map.Strict as Map
import Data.Random

keepMax :: Ord b => ([a], b) -> (a, b) -> ([a], b)
keepMax (selected, maxScore) (action, score) =
    if | score > maxScore -> ([action], score)
       | score == maxScore -> (action:selected, score)
       | otherwise -> (selected, maxScore)

-- TODO: break ties by uniform random sampling
selectMax :: Ord b => [(a, b)] -> RVar a
selectMax [] = error "Cannot select from an empty list"
selectMax scores = randomElement $ fst $ foldl keepMax init scores
  where init = ([fst $ head scores], snd $ head scores)

selectMaxMap :: Ord b => Map a b -> RVar a
selectMaxMap = selectMax . Map.toList

sigmoid :: Floating a => a -> a
sigmoid z = 1 / (1 + exp (-z))
