{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}

module Bandit.Linear where

import           Control.Monad
import           Data.Random                                 hiding (Normal)
import           Data.Random.Distribution.MultivariateNormal
import           Numeric.LinearAlgebra.HMatrix

import           Bandit.Types
import           Bandit.Utils

type Ctx = Vector Double

type Prior actions = [(actions, Normal (Vector Double))]

data LinearCtxBanditTS actions = LinearCtxBanditTS
  { beta  :: Double
  , prior :: Prior actions
  }

instance Eq act => CtxBanditAgent (LinearCtxBanditTS act) Ctx act Double where
  selectActionFromCtx LinearCtxBanditTS {prior} ctx =
    selectActionFromCtxTS prior ctx
  updateAgentWithCtx ctx act reward LinearCtxBanditTS {..} =
    LinearCtxBanditTS beta $ updateAgentWithCtxTS beta ctx act reward prior

selectActionFromCtxTS :: Eq act => Prior act -> Ctx -> RVar act
selectActionFromCtxTS prior ctx = do
  scores <- forM prior sampleScores
  selectMax scores
  where
    sampleScores (action, dist) = do
      weights <- rvar dist
      pure (action, ctx <.> weights)


{-- Update the belief of the agent, with a (context, action, reward) triple. Since
the belief is modeled by a conjugate prior (a joint Gaussian distribution on the
weights), the updating is simple. See for example [Bishop, Section 3.4]. --}
updateAgentWithCtxTS ::
     Eq act => Double -> Ctx -> act -> Double -> Prior act -> Prior act
updateAgentWithCtxTS beta ctx act reward prior = updatePrior `fmap` prior
  where
    updatePrior (a, pr@(Normal mu sigma)) =
      if act == a
        then let lambda = inv $ unSym sigma
                 sigma' = lambda + scalar beta * outer ctx ctx
                 mu' = sigma' #> (lambda #> mu + scalar (beta * reward) * ctx)
             in (a, Normal mu' (sym sigma'))
        else (a, pr)
