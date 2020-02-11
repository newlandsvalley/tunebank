{-# LANGUAGE DeriveGeneric #-}

module Tunebank.Model.NewUser where

import GHC.Generics
import Data.Text (Text)
import Database.PostgreSQL.Simple
-- import Database.PostgreSQL.Simple.FromField (FromField(..), fromField)
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow

-- | a data type that represents a new user to be presented to the database
-- | which will generate default values for id, creation_ts and role (Normal)

data NewUser = NewUser
  { name :: Text
  , email :: Text
  , password :: Text
  } deriving (Eq, Show, Generic)

instance ToRow NewUser where
  toRow u = [ toField (name u), toField (email u), toField (password u) ]

data EmailConfirmation = EmailConfirmation
  { address :: Text
  , slug    :: Text
  }

-- | the returned type of a successful new user registration

instance FromRow EmailConfirmation where
  fromRow = EmailConfirmation <$> field <*> field
