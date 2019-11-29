
{-# LANGUAGE DeriveGeneric #-}

module Tunebank.Model.User where

import Data.Time.Calendar
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Aeson.Parser
import Data.Text (Text, pack, toLower)
import Data.Maybe (Maybe)

data Role =
    Administrator
  | NormalUser
    deriving (Eq, Ord, Show, Generic)

instance ToJSON Role
instance FromJSON Role

-- | A user
data User = User
  { name :: Text
  , email :: Text
  , password :: Text
  , role :: Role
  , registration_date :: Day
  , registered :: Bool
  } deriving (Eq, Show, Generic)

instance ToJSON User

-- needed for tests
instance FromJSON User

-- | A user we'll grab from the database when we authenticate someone
newtype UserName = UserName { userName :: Text }
  deriving (Eq, Show)
