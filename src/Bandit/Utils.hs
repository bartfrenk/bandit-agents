module Bandit.Utils where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Random

keepMax :: Ord b => (a, b) -> (a, b) -> (a, b)
keepMax (selected, maxScore) (action, score) =
    if score > maxScore
    then (action, score)
    else (selected, maxScore)

selectMax :: Ord b => [(a, b)] -> RVar a
selectMax scores = pure $ fst $ foldl1 keepMax scores


selectMaxMap :: Ord b => Map a b -> RVar a
selectMaxMap = selectMax . Map.toList

sigmoid :: Floating a => a -> a
sigmoid z = 1 / (1 + exp (-z))
