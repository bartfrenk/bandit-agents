module Bandit.Utils where

import Data.Random

keepMax :: Ord b => (a, b) -> (a, b) -> (a, b)
keepMax (selected, maxScore) (action, score) =
    if score > maxScore
    then (action, score)
    else (selected, maxScore)

selectMax :: Ord b => [(a, b)] -> RVar a
selectMax scores = pure $ fst $ foldl1 keepMax scores

