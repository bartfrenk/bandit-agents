{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Bandit.Environments.Types where

import           Data.Random (RVar)

class Environment env ctx act rew | env -> ctx act rew where
  generateReward :: env -> ctx -> act -> Maybe (RVar rew)
  generateContext :: env -> RVar ctx
  maximalReward :: env -> ctx -> rew
  expectedReward :: env -> ctx -> act -> rew
