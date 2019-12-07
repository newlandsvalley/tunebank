
{-# LANGUAGE DeriveGeneric #-}

module Tunebank.Model.AbcMetadata where

import Data.Time.Calendar
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Aeson.Parser
import Web.Internal.HttpApiData
import Data.Text (Text, pack, toLower)
import Data.Maybe (Maybe)

-- | a tune represented in ABC notation with the important header metadata
-- | made more easily searchable
data AbcMetadata = AbcMetadata
    { title :: Text
    , key :: Text
    , rhythm :: Text
    , submitter ::Text
    , transcriber :: Maybe Text
    , abc :: Text
    } deriving (Eq, Show, Generic)


instance ToJSON AbcMetadata

instance FromJSON AbcMetadata

-- | a title query parameter
newtype Title = Title Text
  deriving (Eq, Ord, Show, Generic)

-- | this instance supports Capture text of type Title
instance FromHttpApiData Title
  where
    parseUrlPiece t = Right $ Title t

-- required for client testing
instance ToHttpApiData Title
  where
    toUrlPiece (Title t) = t

-- | a rhythm query parameter
newtype Rhythm = Rhythm Text
  deriving (Eq, Ord, Show, Generic)

-- | this instance supports Capture text of type Rhythm
instance FromHttpApiData Rhythm
  where
    parseUrlPiece r = Right $ Rhythm r

-- required for client testing
instance ToHttpApiData Rhythm
  where
    toUrlPiece (Rhythm r) = r

-- | a tune key query parameter
newtype TuneKey = TuneKey Text
  deriving (Eq, Ord, Show, Generic)

-- | this instance supports Capture text of type Rhythm
instance FromHttpApiData TuneKey
  where
    parseUrlPiece k = Right $ TuneKey k

-- required for client testing
instance ToHttpApiData TuneKey
  where
    toUrlPiece (TuneKey k) = k
