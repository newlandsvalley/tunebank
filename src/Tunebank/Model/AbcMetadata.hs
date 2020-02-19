
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | ABC Metadata for a tune and the related query parameters for selecting it

module Tunebank.Model.AbcMetadata where

import Prelude ()
import Prelude.Compat hiding (lookup)

import Data.Time.Calendar
import Data.Time.Clock (UTCTime)
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Aeson.Parser
import Web.Internal.HttpApiData
import Data.Text (Text, pack, toLower)
import Data.Maybe (Maybe)
import Data.Map (Map, fromList, elems, lookup)
import Data.Typeable
import Data.Bifunctor (second, bimap)
import Servant.API.ContentTypes
import Network.HTTP.Media ((//), (/:))
import qualified Data.Text.Encoding as TextS (encodeUtf8, decodeUtf8')
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField (FromField(..), fromField)
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Abc.Serializer (serializeHeaders)
import Data.Abc.Parser (abcParse, headersParse)
import qualified Data.Abc as ABC
import Data.Genre (Genre(..))
import Tunebank.Model.User (UserName(..))
import Tunebank.Utils.Timestamps (fromDay)
import Data.Validation (toEither)
import Data.Abc.Validator (buildHeaderMap, validateHeaders)
import qualified Data.Abc.Validator as V (ValidatedHeaders(..))

-- | a tune represented in ABC notation with the important header metadata
-- | made more easily searchable
data AbcMetadata = AbcMetadata
    { title :: Text
    , rhythm :: Text
    , key :: Text
    , submitter :: Text
    , ts :: Text        -- epoch timestamp - 13 character string
    , abc :: Text
    , source :: Maybe Text
    , origin :: Maybe Text
    , composer :: Maybe Text
    , transcriber :: Maybe Text
    } deriving (Eq, Show, Generic)


instance ToJSON AbcMetadata where
    -- For efficiency, we write a simple toEncoding implementation, as
    -- the default version uses toJSON.
    toEncoding = genericToEncoding defaultOptions

instance FromJSON AbcMetadata

instance FromRow AbcMetadata where
  fromRow = AbcMetadata <$> field <*> field <*> field <*> field <*>
                            field <*> field <*> field <*> field <*>
                            field <*> field

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
data SortKey
  = Alpha
  | Date
    deriving (Eq, Ord, Show, Enum)

instance FromHttpApiData SortKey
  where
    parseUrlPiece s =
      case s of
        "date" -> Right Date
        _ -> Right Alpha

-- required for client testing
instance ToHttpApiData SortKey
  where
    toUrlPiece Date = "date"
    toUrlPiece Alpha = "alpha"


data ABC deriving Typeable

instance Accept ABC where
  contentType _ = "text" // "vnd.abc"

instance MimeRender ABC AbcMetadata where
  mimeRender _ metadata  =
    (fromStrict . TextS.encodeUtf8) $ abc metadata


-- not really needed accept by servant-client
instance MimeUnrender ABC AbcMetadata where
  mimeUnrender _ abcBytes =
    case ((TextS.decodeUtf8' . toStrict) abcBytes) of
      Left err ->
        Left "illegal ABC chars"
      Right abcText ->
        let
          dateString = "06 Feb 2020"
        in
          buildClientMetadata (UserName "fred") dateString Scandi abcText

-- again just to satisfy testing client
buildClientMetadata :: UserName -> Text -> Genre -> Text ->  Either String AbcMetadata
buildClientMetadata (UserName submitter) dateString genre abcText  =
  case (abcParse abcText) of
    Left err ->
      Left err
    Right abc ->
      let
        headerText = serializeHeaders (ABC.headers abc)
        headerMap = buildHeaderMap $ ABC.headers abc
        validated = toEither $ validateHeaders genre headerMap
        source = lookup ABC.Source headerMap
        origin = lookup ABC.Origin headerMap
        composer = lookup ABC.Composer headerMap
        transcriber = lookup ABC.Transcription headerMap
        fromValid :: V.ValidatedHeaders -> AbcMetadata
        fromValid  (V.ValidatedHeaders title _ key rhythm ) =
          AbcMetadata title rhythm key submitter dateString
                      abcText
                      source origin composer transcriber
      in
        bimap concat fromValid validated
