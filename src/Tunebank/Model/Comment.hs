{-# LANGUAGE DeriveGeneric #-}

module Tunebank.Model.Comment where

import GHC.Generics
import Data.Aeson
import Web.Internal.HttpApiData
import Data.Text (Text)
import Tunebank.Model.TuneRef (TuneId)


-- | the unique ID of a tune (within a genre)
newtype CommentId = CommentId Text
  deriving (Eq, Ord, Show, Generic)

instance ToJSON CommentId
instance FromJSON CommentId

-- | this instance supports Capture text of type UserId which are
-- | all in one piece when found in the URL component that validates
-- | a new user request
instance FromHttpApiData CommentId
  where
    parseUrlPiece v = Right $ CommentId v

-- required for client testing
instance ToHttpApiData CommentId
  where
    toUrlPiece (CommentId v) = v

-- | A comment on a tune
data Comment = Comment
  { cid :: CommentId
  , tuneId :: TuneId
  , user :: Text
  , subject :: Text
  , text :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON Comment
instance FromJSON Comment

data CommentList = CommentList
  { comment :: [Comment]
  } deriving (Eq, Show, Generic)


instance ToJSON CommentList
instance FromJSON CommentList
