{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Bandit where

import Control.Monad.Free
import Data.Random.RVar
import Data.Random.Distribution.Beta hiding (beta)
import Data.Maybe
import Data.Random
import Control.Monad.Catch
import System.Random (StdGen, mkStdGen)
import qualified Control.Monad.Trans as MTL
import Data.Map.Strict

import Control.Monad.State


-- data BanditStateF ref prior next
--   = Store ref prior next
--   | Load ref (Maybe prior -> next)
--   deriving (Functor)


-- type BanditStateM ref prior = Free (BanditStateF ref prior)

-- store :: ref -> prior -> BanditStateM ref prior ()
-- store ref prior = liftF $ Store ref prior ()

-- load :: ref -> BanditStateM ref prior (Maybe prior)
-- load ref = liftF $ Load ref id


data BetaParams = BetaParams
  { alpha :: Double
  , beta :: Double
  }

-- type BetaState = BanditStateM Int BetaParams

-- selectBeta :: Int -> ExceptT String (RVarT BetaState) Double
-- --selectAction = undefined
-- selectBeta ref = do
--   v <- lift $ lift $ load ref
--   case v of
--     Nothing -> throwError "ref unknown"
--     Just BetaParams{..} -> do
--       lift $ betaT alpha beta


-- interpret :: BetaState a -> IO a
-- interpret = undefined

class AgentState prior m where
  store :: prior -> m ()
  load :: m (Maybe prior)

-- selectBeta' :: MonadState (Map String BetaParams) m => RVarT m String
-- selectBeta' = do
--   params <- lift $ List <$> get
--   betaT alpha beta

--type Bandit m prior action = RVarT (m prior) action

-- class BanditAgent agent reward where
--   selectAction :: agent -> action
--   updateAgent :: agent -> action -> reward -> agent

runBandit :: state -> RVarT (State state) action -> IO action
runBandit s action = runRVarTWith f action $ StdRandom
  where f = return . flip evalState s


-- rwalkState :: RVarT (State Double) Double
-- rwalkState = do
--     prev <- lift get
--     change  <- rvarT StdNormal

--     let new = prev + change
--     lift (put new)
--     return new


-- rwalk :: Int -> Double -> StdGen -> ([Double], StdGen)
-- rwalk count start gen =
--     flip evalState start .
--         flip runStateT gen .
--             runRVarTWith MTL.lift $
--                 replicateM count rwalkState





-- runBandit :: RandomSource m StdRandom => RVarT BetaState a -> m a
-- runBandit action = do
--   x <- fmap interpret action
--   sampleRVarTWith lift x StdRandom
-- --stateInterpreter :: BanditStateF -> 
