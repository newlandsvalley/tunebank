
{-# LANGUAGE DeriveGeneric #-}

module Tunebank.Model.NewTune where

import Data.Time.Calendar
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Aeson.Parser
import Data.Text (Text)
import Web.FormUrlEncoded

data Submission = Submission
  { abc :: Text
  } deriving (Eq, Show, Generic)

instance FromForm Submission

-- required for testing (somehow)
instance ToForm Submission
