module Bandit.Agents.Linear where

import           Control.Monad
import           Data.Random                                 hiding (Normal)
import           Data.Random.Distribution.MultivariateNormal
import           Numeric.LinearAlgebra.HMatrix

import           Bandit.Agents.Types
import           Bandit.Utils

type Ctx = Vector Double

type Prior actions = [(actions, Normal (Vector Double))]

{- |Bandit agent in the environment where the expected reward is a linear
function of the context, and the errors are normally distributed with precision
`beta`. The `prior` field maps actions to priors on the linear coefficients for
the expected reward functions of these actions.
-}
data LinearCtxBanditTS actions = LinearCtxBanditTS
  { beta  :: !Double
  , prior :: !(Prior actions)
  }

instance Eq act => BanditAgent (LinearCtxBanditTS act) Ctx act Double where
  selectAction LinearCtxBanditTS {prior} ctx = selectActionFromCtxTS prior ctx
  updateBelief ctx act reward LinearCtxBanditTS {..} =
    LinearCtxBanditTS beta $ updateAgentWithCtxTS beta ctx act reward prior

selectActionFromCtxTS :: Eq act => Prior act -> Ctx -> RVar act
selectActionFromCtxTS prior ctx = do
  scores <- forM prior sampleScores
  selectMax scores
  where
    sampleScores (action, dist) = do
      weights <- rvar dist
      pure (action, ctx <.> weights)

{- |
Update the belief of the agent, with a (context, action, reward) triple. Since
the belief is modeled by a conjugate prior (a joint Gaussian distribution on the
weights), the updating is simple. See, for example [Bishop, Section 3.4].
-}
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
