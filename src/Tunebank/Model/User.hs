
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Tunebank.Model.User where

import Data.Time.Calendar
import GHC.Generics
import Data.Aeson
import Web.Internal.HttpApiData
import Data.Bifunctor (bimap)
import Data.Text (Text, pack, unpack)
import Text.Read (readEither)
import Tunebank.Model.Pagination (Pagination(..))
import Database.PostgreSQL.Simple.FromField (FromField(..), fromField)
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow

data Role =
    Administrator
  | NormalUser
    deriving (Eq, Ord, Show, Generic)

instance ToJSON Role
instance FromJSON Role


instance ToField Role  where
  toField r = toField (show r)

instance FromField Role where
  fromField fld mdata = do
    x <- fromField fld mdata
    case x :: Text of
      "Admin" -> return Administrator
      _ -> return NormalUser


newtype UserId = UserId { userId :: Int }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON UserId

instance FromJSON UserId

-- | this instance supports Capture text of type UserId which are
-- | all in one piece when found in the URL component that validates
-- | a new user request
instance FromHttpApiData UserId
  where
    parseUrlPiece = readTextAsUserId
    --  parseUrlPiece u = Right $ UserId (readTextAsInt u)

-- required for client testing
instance ToHttpApiData UserId
  where
    toUrlPiece (UserId u) = pack $ show u

instance FromField UserId where
  fromField fld bs = UserId <$> fromField fld bs

instance ToField UserId  where
  toField (UserId u) = toField u

readTextAsUserId :: Text -> (Either Text UserId)
readTextAsUserId t =
  bimap pack UserId $
    readEither (unpack t)


-- | A user
data User = User
  { uid :: UserId
  , name :: Text
  , email :: Text
  , password :: Text
  , role :: Role
  , registration_date :: Day
  , valid :: Bool
  } deriving (Eq, Show, Generic)

instance ToJSON User

-- needed for tests
instance FromJSON User

instance FromRow User where
    fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow User where
    toRow u = [ toField (uid u), toField (name u), toField (email u)
              , toField (password u), toField (role u)
              , toField (registration_date u), toField (valid u)
              ]

-- | A user we'll grab from the database when we authenticate someone
newtype UserName = UserName Text
  deriving (Eq, Ord, Show, Generic)

instance ToJSON UserName

instance FromJSON UserName

instance FromField UserName where
  fromField field bs = UserName <$> fromField field bs

instance ToField UserName where
  toField (UserName u) = toField u

data UserList = UserList
  { users :: [User]
  , pagination :: Pagination
  }
    deriving (Eq, Show, Generic)

instance ToJSON UserList
instance FromJSON UserList
