
{-# LANGUAGE DeriveGeneric #-}

module Tunebank.Model.UserRegistration where

import Data.Time.Calendar
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Aeson.Parser
import Data.Maybe (Maybe)
import Data.Text (Text, pack)
import Web.FormUrlEncoded

data Submission = Submission
  { name :: Text
  , email :: Text
  , password :: Text
  , password2 :: Text
  , refererUrl :: Text
  } deriving (Eq, Show, Generic)

instance FromForm Submission

-- required for testing (somehow)
instance ToForm Submission
