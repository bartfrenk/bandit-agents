module Bandit where


data BanditTerms id prior next
  = Store prior next
  | Load id (prior -> next)
  deriving (Show, Functor)



