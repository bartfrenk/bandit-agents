{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}

module Bandit.Agents.Logistic where

import           Data.Map.Strict                             (Map)
import qualified Data.Map.Strict                             as Map
import           Data.Random                                 (sample)
import           Data.Random.Distribution.MultivariateNormal (Normal (..))
import           Numeric.GSL.Minimization                    (MinimizeMethodD (..),
                                                              minimizeVD)
import           Numeric.LinearAlgebra.HMatrix

import           Bandit.Agents.Types
import           Bandit.Utils

newBanditTS :: Ord act => Int -> [act] -> LogisticBanditTS act
newBanditTS k acts = LogisticBanditTS $ Map.fromList $ zip acts (repeat prior)
  where
    prior = (Normal $! (k |> repeat 0.0)) $! (sym $ ident k)

{- | Bandit agent for the setting where the reward for an action is a Bernoulli
random variable whose expected value is the sigmoid of a linear combination of
the contexts.

Remark: Implements the logistic regression model from [Chapelle, Li, 2011], described
in the 'News article recommendation section', in which each action has its own
logistic regression model. We do not need to assume the set of actions is fixed,
but each time that we are presented with a new action, maybe via some extraneous
`addAction` function, the prior on the weights is set to some fixed prior.

The method for implementing the updateActionWithCtx is listed in the `Display
Advertising` section, and is approximate (there is no useful way of representing
the posterior). It approximates the posterior by its Laplace approximation, at
each step.
--}
data LogisticBanditTS act = LogisticBanditTS
  { prior :: !(Map act (Normal (Vector Double)))
  }

instance Ord act => BanditAgent (LogisticBanditTS act) Ctx act Double where
  selectAction (LogisticBanditTS prior) ctx = do
    variates <- mapM sample prior
    let scores = sigmoid . (ctx <.>) <$> variates
    selectMaxMap scores
  updateBelief ctx act rew (LogisticBanditTS prior) =
    let batch = Batch (vector [rew]) (asRow ctx)
    in LogisticBanditTS $ Map.adjust (approximatePosterior batch) act prior

instance Ord act => BatchUpdateBelief (LogisticBanditTS act) (act, Batch) where
  batchUpdateBelief (act, batch) (LogisticBanditTS prior) =
    LogisticBanditTS $ Map.adjust (approximatePosterior batch) act prior

type Ctx = Vector Double

logPosterior' ::
     Vector Double -- ^ vector of target values, 0, or 1, of length n
  -> Matrix Double -- ^ matrix of contexts, with n columns
  -> Normal (Vector Double) -- ^ the prior
  -> Vector Double -- ^ weights of logistic regression
  -> Double
logPosterior' targets ctxs (Normal mean covariance) w =
  let d = w - mean
      precision = inv $ unSym covariance -- probably better to keep the
                                         -- precision with the prior
      regularizer = -0.5 * d <.> (precision #> d)
      activations = 1 / (1 + exp (-(ctxs #> w)))
      -- factor to avoid -Infinity appearing in the expression
      loss =
        targets <.> (log (max activations tol)) +
        (1 - targets) <.> (log (max (1 - activations) tol))
      tol = size activations |> repeat 1e-30
  in loss + regularizer

gradLogPosterior' ::
     Vector Double
  -> Matrix Double
  -> Normal (Vector Double)
  -> Vector Double
  -> Vector Double
gradLogPosterior' targets ctxs (Normal mean covariance) w =
  let d = w - mean
      precision = inv $ unSym covariance
      regularizer = -precision #> d
      activations = 1 / (1 + exp (-(ctxs #> w)))
      coeffs = targets * (1 - activations) - (1 - targets) * activations
      loss = coeffs <# ctxs
  in loss + regularizer

data Batch = Batch
  { targets :: Vector Double
  , ctxs    :: Matrix Double
  } deriving (Show)

logPosterior :: Normal (Vector Double) -> Batch -> Vector Double -> Double
logPosterior prior Batch {..} = logPosterior' targets ctxs prior

gradLogPosterior ::
     Normal (Vector Double) -> Batch -> Vector Double -> Vector Double
gradLogPosterior prior Batch {..} = gradLogPosterior' targets ctxs prior

posteriorMode ::
     Normal (Vector Double) -> Batch -> (Vector Double, Matrix Double)
posteriorMode prior batch =
  minimizeVD VectorBFGS2 1E-2 10 0.1 1E-2 fn grad startingPoint
  where
    fn = negate . (logPosterior prior batch)
    grad = negate . (gradLogPosterior prior batch)
    n = cols $ ctxs batch
  --      searchBox = fromList (take n $ repeat 1.0)
  -- seems to make more sense to take the mean of the prior
    startingPoint = fromList (take n $ repeat 0.0)

-- | Compute the Laplace approximation to the posterior over the weights of the
-- logistic regression model.
approximatePosterior ::
     Batch -> Normal (Vector Double) -> Normal (Vector Double)
approximatePosterior batch@Batch {..} prior@(Normal mean covariance) =
  let mean' = fst (posteriorMode prior batch)
      precision = inv $ unSym covariance
      ys = 1 / (1 + exp (-(ctxs #> mean)))
      zs = ys * (1 - ys)
      comp n = (scalar (zs ! n)) * (outer (ctxs ! n) (ctxs ! n))
      precision' = foldl (+) precision (comp `fmap` [0 .. (size ys) - 1])
  in (Normal $! mean') $! (sym $ inv precision')
