{-# LANGUAGE DeriveGeneric #-}

-- | Probably needs renaming
-- | this represents the data structure of a comment submitted by the user
-- | and also represent the structure when a list of commenta against a tune
-- | is returned to the user

module Tunebank.Model.CommentSubmission where

import GHC.Generics
import Data.Aeson
import Web.FormUrlEncoded
import Data.Text (Text)
import Tunebank.Model.Comment (CommentId)
-- import Database.PostgreSQL.Simple.FromField (FromField(..), fromField)
import Database.PostgreSQL.Simple.FromRow

-- | A mew comment submitted on a tune
data Submission = Submission
  { user :: Text
  , commentId :: CommentId
  , subject :: Text
  , text :: Text
  } deriving (Eq, Show, Generic)

instance FromForm Submission

-- required for testing (somehow)
instance ToForm Submission

instance FromRow Submission where
  fromRow = Submission <$> field <*> field <*> field <*> field

instance ToJSON Submission

-- just to satisfy client
instance FromJSON Submission
