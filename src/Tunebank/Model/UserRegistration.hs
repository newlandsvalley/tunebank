
{-# LANGUAGE DeriveGeneric #-}

module Tunebank.Model.UserRegistration where

import GHC.Generics
import Data.Text (Text)
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
