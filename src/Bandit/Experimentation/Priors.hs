{-# LANGUAGE MultiParamTypeClasses #-}

module Bandit.Experimentation.Priors where

import           Data.Random.Distribution.Beta

class ConjugatePrior prior variates where
  updatePrior :: variates -> prior -> prior

instance ConjugatePrior (Beta a) Double where
  updatePrior = undefined

class Scoreable scoreable score m where
  computeScore :: scoreable -> m score

-- instance (ConjugatePrior (prior score) variates, Distribution prior score) =>
--          Scoreable (prior score) score RVar

-- data Bandit
