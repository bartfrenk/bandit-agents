{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Bandit.Linear where

import           Control.Monad
import           Data.Random                                 hiding (Normal)
import           Data.Random.Distribution.MultivariateNormal
import           Numeric.LinearAlgebra.HMatrix

import           Bandit.Types
import           Bandit.Utils

data LinearCtxBanditTS actions =
  LinearCtxBanditTS [(actions, Normal (Vector Double))]

type Ctx = Vector Double

instance Eq act => CtxBanditAgent (LinearCtxBanditTS act) Ctx act Double where
  selectActionFromCtx (LinearCtxBanditTS prior) ctx =
    selectActionFromCtxTS prior ctx
  updateAgentWithCtx ctx act reward agent = undefined

selectActionFromCtxTS ::
     Eq act => [(act, Normal (Vector Double))] -> Vector Double -> RVar act
selectActionFromCtxTS prior ctx = do
  scores <- forM prior sampleScores
  selectMax scores
  where
    sampleScores (action, dist) = do
      weights <- rvar dist
      pure (action, ctx <.> weights)


