{-# LANGUAGE DeriveGeneric #-}

-- | this represent a comment as stored on the database

module Tunebank.Model.Comment where

import GHC.Generics
import Data.Aeson
import Web.Internal.HttpApiData
import Data.Bifunctor (bimap)
import Data.Text (Text, pack, unpack)
import Text.Read (readEither)
import Tunebank.Model.TuneRef (TuneId)
import Database.PostgreSQL.Simple.FromField (FromField(..), fromField)
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow


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

instance FromField CommentId where
  fromField fld bs = CommentId <$> fromField fld bs

instance ToField CommentId  where
  toField (CommentId t) = toField t

-- | A comment on a tune
data Comment = Comment
  { commentId :: CommentId
  , tidkey :: Int
  , submitter :: Text
  , subject :: Text
  , text :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON Comment
instance FromJSON Comment

instance FromRow Comment where
  fromRow = Comment <$> field <*> field <*> field <*> field <*> field

instance ToRow Comment where
  toRow c = [ toField (commentId c), toField (tidkey c), toField (submitter c), toField (subject c), toField (text c) ]
