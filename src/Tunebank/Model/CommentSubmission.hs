{-# LANGUAGE DeriveGeneric #-}

module Tunebank.Model.CommentSubmission where

import GHC.Generics
import Web.Internal.HttpApiData
import Web.FormUrlEncoded
import Data.Text (Text)
import Tunebank.Model.TuneRef (TuneId)
import Tunebank.Model.Comment (CommentId)

-- | A mew comment submitted on a tune
data Submission = Submission
  { cid :: CommentId
  , user :: Text
  , subject :: Text
  , text :: Text
  } deriving (Eq, Show, Generic)

instance FromForm Submission

-- required for testing (somehow)
instance ToForm Submission
