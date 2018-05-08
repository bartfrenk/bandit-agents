module Bandit.Logistic where


import Control.Monad
import           Data.Map.Strict                             (Map)
import Data.Random hiding (Normal)
import Data.Random.Distribution.Bernoulli
import           Data.Random.Distribution.MultivariateNormal
import           Numeric.GSL.Minimization
import           Numeric.LinearAlgebra.HMatrix

import Debug.Trace
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy hiding (Vector, Matrix, (<.>), (??), (|>))

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

testMinimizeD = minimizeD VectorBFGS2 1E-2 30 0.1 1E-2 fn grad [1000.0, 1000.0]
  where
    fn [x, y] = (x - 10) ** 2 + (y - 12) ** 2
    grad [x, y] = [2 * (x - 10), 2 * (y - 12)]

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
      loss = targets <.> (log (max activations tol)) +
             (1 - targets) <.> (log (max (1 - activations) tol))
      tol = size activations |> repeat 1e-30
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
      coeffs = targets * (1 - activations) - (1 - targets) * activations
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
  minimizeVD VectorBFGS2 1E-2 10 0.1 1E-2 fn grad startingPoint
  where fn = negate . (logPosterior prior batch)
        grad = negate . (gradLogPosterior prior batch)
        n = ctxDimension batch
  --      searchBox = fromList (take n $ repeat 1.0)
        startingPoint = fromList (take n $ repeat 0.0)


posteriorCovariance prior@(Normal mean covariance) batch@Batch{..} =
  let w = fst $ posteriorMode prior batch
      activations = 1 / (1 + exp(-(ctxs #> w)))
      coeffs = activations * (1 - activations)
      ctxRows = toRows ctxs
  in undefined

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
randomContext k = sample $ Normal (fromList $ take k $ repeat 0.0)  (sym $ ident k)

testPosteriorMode :: Vector Double -> Int -> RVar (Vector Double, Matrix Double)
testPosteriorMode w n = do
  let k = size w
  let prior = Normal (fromList $ take k $ repeat 0.0) (sym $ 1000 * ident k)
  batch <- randomBatch (randomContext k) w n
  pure $ (posteriorMode prior batch)

test :: Int -> IO (Vector Double, Matrix Double)
test n = sample $ testPosteriorMode (vector [1.0, 2.0, 3.0, 4.0]) n

-- testMinizeD = minimizeD VectorBFGS2 1E-2 30 0.1 fn grad startingPoint
--   where
--     fn = negate . logPosterior
--     grad = negate . gradLogPosterior
--     startingPoint = undefined

h x = x


testPlot path w n = do
  let k = 1
  let prior = Normal (fromList $ take k $ repeat 0.0) (sym $ ident k)
  batch <- sample (randomBatch (randomContext k) (vector [w]) n)
  plotPosterior path prior batch

plotPosterior :: String -> Normal (Vector Double) -> Batch -> IO ()
plotPosterior path prior batch = do
  let r = range' (-5.0) 5.0 0.1
  let fn = logPosterior prior batch
  -- let fn v = let [x] = toList v
  --            in x**2
  let points = r `zip` (fn . toVec <$> r)
--  putStrLn $ "POINTS: " ++ show (take 2 points)
  toFile def path $ do
    plot $ (line "" $ [points])
  where toVec d = vector [d]


range' :: Double -> Double -> Double -> [Double]
range' start end step = takeWhile (< end) $ iterate (+ step) start
