
{-# LANGUAGE DeriveGeneric #-}

module Tunebank.Model.User where

import Data.Time.Calendar
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Aeson.Parser
import Web.Internal.HttpApiData
import Data.Text (Text, pack, toLower)
import Data.Maybe (Maybe)
import Tunebank.Model.Pagination (Pagination(..))

data Role =
    Administrator
  | NormalUser
    deriving (Eq, Ord, Show, Generic)

instance ToJSON Role
instance FromJSON Role

-- | A user
data User = User
  { name :: Text
  , email :: Text
  , password :: Text
  , role :: Role
  , registration_date :: Day
  , registered :: Bool
  , uid :: UserId
  } deriving (Eq, Show, Generic)

instance ToJSON User

-- needed for tests
instance FromJSON User

-- | A user we'll grab from the database when we authenticate someone
newtype UserName = UserName Text
  deriving (Eq, Ord, Show, Generic)

instance ToJSON UserName

instance FromJSON UserName

-- | We'll eventually nominate a User ID when we have a database layer
newtype UserId = UserId { userId :: Text }
  deriving (Eq, Ord, Show, Generic)


instance ToJSON UserId

instance FromJSON UserId

-- | this instance supports Capture text of type UserId which are
-- | all in one piece when found in the URL component that validates
-- | a new user request
instance FromHttpApiData UserId
  where
    parseUrlPiece u = Right $ UserId u

-- required for client testing
instance ToHttpApiData UserId
  where
    toUrlPiece (UserId u) = u

data UserList = UserList
  { users :: [User]
  , pagination :: Pagination
  }
    deriving (Eq, Show, Generic)

instance ToJSON UserList
instance FromJSON UserList
