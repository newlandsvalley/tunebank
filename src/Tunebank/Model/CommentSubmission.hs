{-# LANGUAGE DeriveGeneric #-}

module Tunebank.Model.CommentSubmission where

import GHC.Generics
import Web.FormUrlEncoded
import Data.Text (Text)
import Tunebank.Model.Comment (CommentId)
-- | A mew comment submitted on a tune
data Submission = Submission
  { user :: Text
  , cid :: CommentId
  , title :: Text
  , text :: Text
  } deriving (Eq, Show, Generic)

instance FromForm Submission

-- required for testing (somehow)
instance ToForm Submission
