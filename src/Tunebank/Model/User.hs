
{-# LANGUAGE DeriveGeneric #-}

module Tunebank.Model.User where

import Data.Time.Calendar
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Aeson.Parser
import Data.Text (Text, pack, toLower)
import Data.Maybe (Maybe)

-- | A user
data User = User
  { name :: String
  , email :: String
  , role :: String
  , registration_date :: Day
  , registered :: Bool
  } deriving (Eq, Show, Generic)

instance ToJSON User
