module Bandit.Environments.Logistic where

import           Data.Map.Strict                             (Map, (!), (!?))
import qualified Data.Map.Strict                             as Map
import           Data.Random                                 hiding (Normal)
import           Data.Random.Distribution.Bernoulli
import           Data.Random.Distribution.MultivariateNormal
import           Numeric.LinearAlgebra                       hiding ((!))

import           Bandit.Environments.Types
import           Bandit.Utils                                (sigmoid)

newLogisticEnv :: Ord act => [(act, Vector Double)] -> LogisticEnv act
newLogisticEnv weights =
  let dim = size $ snd $ head weights
  in LogisticEnv {weights = Map.fromList weights, dim = dim}

data LogisticEnv act = LogisticEnv
  { weights :: Map act (Vector Double)
  , dim     :: Int
  }

instance Ord act =>
         Environment (LogisticEnv act) (Vector Double) act Double where
  generateReward LogisticEnv {weights} ctx act =
    bernoulli . sigmoid . (ctx <.>) <$> weights !? act
  generateContext LogisticEnv {dim} = randomContext dim
  maximalReward LogisticEnv {weights} ctx =
    let exp w e = max (sigmoid $ w <.> ctx) e
    in Map.foldr exp 0 weights
  expectedReward LogisticEnv {weights} ctx act =
    sigmoid (ctx <.> (weights ! act))

randomContext :: Int -> RVar (Vector Double)
randomContext k =
  sample $ Normal (fromList $ take k $ repeat 0.0) (sym $ ident k)
