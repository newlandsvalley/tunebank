
{-# LANGUAGE DeriveGeneric #-}

-- | ABC Metadata for a tune and the related query parameters for selecting it

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
    , source :: Maybe Text
    , origin :: Maybe Text
    , composer :: Maybe Text
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
    parseUrlPiece = Right . Title

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
    parseUrlPiece = Right . Rhythm

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
    parseUrlPiece = Right . TuneKey

-- required for client testing
instance ToHttpApiData TuneKey
  where
    toUrlPiece (TuneKey k) = k

-- | a tune source query parameter
newtype Source = Source Text
  deriving (Eq, Ord, Show, Generic)

-- | this instance supports Capture text of type Rhythm
instance FromHttpApiData Source
  where
    parseUrlPiece  = Right . Source

-- required for client testing
instance ToHttpApiData Source
  where
    toUrlPiece (Source v) = v

-- | a tune origin query parameter
newtype Origin = Origin Text
  deriving (Eq, Ord, Show, Generic)

-- | this instance supports Capture text of type Rhythm
instance FromHttpApiData Origin
  where
    parseUrlPiece  = Right . Origin

-- required for client testing
instance ToHttpApiData Origin
  where
    toUrlPiece (Origin v) = v

-- | a tune composer query parameter
newtype Composer =  Composer Text
  deriving (Eq, Ord, Show, Generic)

-- | this instance supports Capture text of type Rhythm
instance FromHttpApiData  Composer
  where
    parseUrlPiece  = Right . Composer

-- required for client testing
instance ToHttpApiData Composer
  where
    toUrlPiece (Composer v) = v

-- | a tune transcriber query parameter
newtype Transcriber =  Transcriber Text
  deriving (Eq, Ord, Show, Generic)

-- | this instance supports Capture text of type Rhythm
instance FromHttpApiData Transcriber
  where
    parseUrlPiece  = Right . Transcriber

-- required for client testing
instance ToHttpApiData Transcriber
  where
    toUrlPiece (Transcriber v) = v

-- | Sort Criteria
data Sort
  = Alpha
  | Date
    deriving (Eq, Ord, Show, Enum)    
