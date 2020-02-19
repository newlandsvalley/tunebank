
{-# LANGUAGE DeriveGeneric #-}

module Tunebank.Model.TuneRef where

import Data.Time.Calendar
import Data.Time.LocalTime (LocalTime)
import Data.Time.Clock (UTCTime)
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Aeson.Parser
import Web.Internal.HttpApiData
import Data.Text (Text, pack, unpack, toLower)
import Data.Either (Either(..))
import Data.Char (isAlphaNum)
import Tunebank.Model.Pagination (Pagination(..))
import Database.PostgreSQL.Simple.FromField (FromField(..), fromField)
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow


-- | the unique ID of a tune (within a genre)
newtype TuneId = TuneId Text
  deriving (Eq, Ord, Show, Generic)

instance ToJSON TuneId
instance FromJSON TuneId

instance FromField TuneId where
  fromField field bs = TuneId <$> fromField field bs

instance ToField TuneId  where
  toField (TuneId t) = toField t



-- | this instance supports Capture text of type TuneId which are
-- | all in one piece when found in URL components
instance FromHttpApiData TuneId
  where
    parseUrlPiece t = Right $ TuneId t

-- required for client testing
instance ToHttpApiData TuneId
  where
    toUrlPiece (TuneId t) = t

-- | but a TuneId is built by combining the title and rhythm
tuneId :: Text -> Text -> TuneId
tuneId title rhythm =
  TuneId ((toLower title) <> (pack "-") <> (toLower rhythm))

safeFileName :: TuneId -> String
safeFileName (TuneId t) =
  filter isAlphaNum (unpack t)

{-}
-- | this is data returned within tune lists
data TuneRef = TuneRef
   { uri   :: TuneId
   , title :: Text
   , rhythm :: Text
   , abcHeaders :: Text
   , abc :: Text    -- the body
   , ts  :: Day
   } deriving (Eq, Show, Generic)
-}
data TuneRef = TuneRef
   { uri :: TuneId
   , title :: Text
   , rhythm :: Text
   , abc :: Text    -- the body
   , ts  :: Text    -- the epoch 13-character timestamp
   } deriving (Eq, Show, Generic)

instance ToJSON TuneRef

instance FromJSON TuneRef

instance FromRow TuneRef where
  fromRow = TuneRef <$> field <*> field <*> field <*> field <*> field

data TuneList = TuneList
  { tunes :: [TuneRef]
  , pagination :: Pagination
  }
    deriving (Eq, Show, Generic)

instance ToJSON TuneList
instance FromJSON TuneList
