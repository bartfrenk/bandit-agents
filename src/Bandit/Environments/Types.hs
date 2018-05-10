{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bandit.Environments.Types where

import Control.Monad.Writer
import Control.Monad.Trans (liftIO, MonadIO)
import qualified Control.Monad.Trans as T
import qualified Data.Random.Lift as R
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Random

import Bandit.Types

class Environment env ctx act rew | env -> ctx act rew where
  generateReward :: env -> ctx -> act -> Maybe (RVar rew)
  generateContext :: env -> RVar ctx
  maximalReward :: env -> ctx -> rew
  expectedReward :: env -> ctx -> act -> rew

