module Bandit.Logistic where

import           Numeric.LinearAlgebra.HMatrix

type Prior actions = [(actions, Normal (Vector Double))]

type Ctx = Vector Double

data LogisticBanditTS action = LogisticBanditTS
  { prior :: Prior actions }


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

instance Eq act => (LogisticBanditTS act) Ctx act Bool where
  selectActionFromCtx = undefined
  updateActionWithCtx = undefined


{-- Implements the logistic regression model from [Chapelle, Li, 2011], described
in the 'Display advertising' section. Difference with `LogisticBanditTS` is that
the action is mapped to a numeric feature vector. Its combination with the
context is the input for a logistic regression model. Thus, we need to keep only
a single logistic regression model in the state.

The update action uses the same approximation techniques as `LogisticBanditTS`.
--}
type FM action = action -> Vector Double

data LogisticBanditFMTS action (FM action) = LogisticBanditFMTS where
  selectActionFromCtx = undefined
  updateActionWithCtx = undefined
