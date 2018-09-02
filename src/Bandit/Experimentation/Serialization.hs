module Bandit.Experimentation.Serialization where

import           Data.ByteString
import           Data.Dynamic
import           Data.Typeable
import           Data.Yaml
import           GHC.Generics

import           Bandit.Agents.Bernoulli

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
