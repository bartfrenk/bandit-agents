module Bandit.Logistic where


import Control.Monad
import           Data.Map.Strict                             (Map)
import Data.Random hiding (Normal)
import Data.Random.Distribution.Bernoulli
import           Data.Random.Distribution.MultivariateNormal
import           Numeric.GSL.Minimization
import           Numeric.LinearAlgebra.HMatrix

type Ctx = Vector Double

data LogisticBanditTS act = LogisticBanditTS
  { prior :: Map act (Normal (Vector Double))
  }

{-- Implements the logistic regression model from [Chapelle, Li, 2011], described
in the 'News article recommendation section', in which each action has its own
logistic regression model. We do not need to assume the set of actions is fixed,
but each time that we are presented with a new action, maybe via some extraneous
`addAction` function, the prior on the weights is set to some fixed prior.

The method for implementing the updateActionWithCtx is listed in the `Display
Advertising` section, and is approximate (there is no useful way of representing
the posterior). It approximates the posterior by its Laplace approximation, at
each step.
--}
-- instance Eq act => (LogisticBanditTS act) Ctx act Bool where
--   selectActionFromCtx = undefined
--   updateActionWithCtx = undefined
{-- Implements the logistic regression model from [Chapelle, Li, 2011], described
in the 'Display advertising' section. Difference with `LogisticBanditTS` is that
the action is mapped to a numeric feature vector. Its combination with the
context is the input for a logistic regression model. Thus, we need to keep only
a single logistic regression model in the state.

The update action uses the same approximation techniques as `LogisticBanditTS`.
--}
-- type FM action = action -> Vector Double
-- data LogisticBanditFMTS action (FM action) = LogisticBanditFMTS where
--   selectActionFromCtx = undefined
--   updateActionWithCtx = undefined
testMinimize = fst $ uniMinimize GoldenSection 1E-2 50 f 3.0 (-10.0) 10.0
  where
    f :: Double -> Double
    f x = x ** 2

testMinimizeD = minimizeD VectorBFGS2 1E-2 30 0.1 1E-2 fn grad [3.0]
  where
    fn [x] = (x - 10) ** 2
    grad [x] = [2 * (x - 10)]

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
      loss = targets <.> activations + (1 - targets) <.> (1 - activations)
  in loss + regularizer


{--
Build up the derivative using the following facts:
1. grad x^T S x = (S + S^T) x
2. grad \sigma(w^T x) = sigma(w^T x) (1 - sigma(w^T x) x
--}

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
      coeffs = targets * activations + (1 - targets) * activations
      loss = coeffs <# ctxs
  in loss + regularizer

data Batch = Batch
  { targets :: Vector Double,
    ctxs :: Matrix Double
  } deriving (Show)


logPosterior :: Normal (Vector Double) -> Batch -> Vector Double -> Double
logPosterior prior Batch{..} =
  logPosterior' targets ctxs prior

gradLogPosterior :: Normal (Vector Double) -> Batch -> Vector Double -> Vector Double
gradLogPosterior prior Batch{..} =
  gradLogPosterior' targets ctxs prior


ctxDimension :: Batch -> Int
ctxDimension Batch{ctxs} = cols ctxs

posteriorMode :: Normal (Vector Double) -> Batch -> (Vector Double, Matrix Double)
posteriorMode prior batch =
  minimizeVD VectorBFGS2 1E-2 30 0.1 0.1 fn grad startingPoint
  where fn = logPosterior prior batch
        grad = gradLogPosterior prior batch
        n = ctxDimension batch
        startingPoint = fromList (take n $ repeat 0.0)


randomBatch :: RVar (Vector Double) -> Vector Double -> Int -> RVar Batch
randomBatch randomCtx w n = do
  points <- replicateM n (randomPoint randomCtx w)
  let ctxs = fromRows (fst <$> points)
  let targets = vector (snd <$> points)
  pure $ Batch { targets = targets, ctxs = ctxs }

randomPoint :: RVar (Vector Double) -> Vector Double -> RVar (Vector Double, Double)
randomPoint randomCtx w = do
  ctx <- randomCtx
  let p = 1.0 / (1.0 + exp(-ctx <.> w))
  target <- bernoulli p
  pure (ctx, target)

randomContext :: Int -> RVar (Vector Double)
randomContext k = sample $ Normal (fromList $ take k $ repeat 0.0) (sym $ ident k)

testPosteriorMode :: Int -> Int -> Vector Double
testPosteriorMode n k =
  let prior = Normal (fromList $ take k $ repeat 0.0) (sym $ ident k)
  in undefined

-- testMinizeD = minimizeD VectorBFGS2 1E-2 30 0.1 fn grad startingPoint
--   where
--     fn = negate . logPosterior
--     grad = negate . gradLogPosterior
--     startingPoint = undefined

h x = x

