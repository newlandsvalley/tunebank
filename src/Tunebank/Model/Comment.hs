{-# LANGUAGE DeriveGeneric #-}

module Tunebank.Model.Comment where

import Data.Time.Calendar
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Aeson.Parser
import Web.Internal.HttpApiData
import Data.Text (Text)

-- | the unique ID of a tune (within a genre)
newtype CommentId = CommentId Text
  deriving (Eq, Ord, Show, Generic)

instance ToJSON CommentId

-- | this instance supports Capture text of type CommentId w
instance FromHttpApiData CommentId
  where
    parseUrlPiece t = Right $ CommentId t

-- | A comment on a tune
data Comment = Comment
  { id :: CommentId
  , user :: Text
  , subject :: Text
  , text :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON Comment
