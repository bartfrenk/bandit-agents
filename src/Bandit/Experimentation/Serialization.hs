{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bandit.Experimentation.Serialization where

import           Data.ByteString
import           Data.Dynamic
import           Data.Typeable
import           Data.Yaml
import           GHC.Generics

import           Bandit.Agents.Types

data Metadata = Metadata
  { schema :: String
  } deriving (Generic)

instance FromJSON Metadata

data WithMeta d = WithMeta
  { _meta :: !Metadata
  , _data :: !d
  }

instance FromJSON d => FromJSON (WithMeta d) where
  parseJSON =
    withObject "WithMeta" $ \obj -> WithMeta <$> obj .: "meta" <*> obj .: "data"

-- |Function to run on a generic agent.
type RunAgent ctx act rew summary
   = (forall agent. BanditAgent agent ctx act rew =>
                      agent -> summary)
