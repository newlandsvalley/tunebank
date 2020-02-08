{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}


module Tunebank.Model.AbcMetadataSubmission where


import Prelude ()
import Prelude.Compat hiding (lookup)

import GHC.Generics
import Data.Text (Text, pack, unpack, toLower)
import Data.Map (Map, fromList, elems, lookup)
import Data.Time.Calendar
import Data.Time.Clock (UTCTime)
import Data.Genre
import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField (FromField(..), fromField)
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Data.Bifunctor (second, bimap)
import qualified Tunebank.Model.User as U (UserName(..))
import qualified Tunebank.Model.TuneRef as TuneRef
import qualified Data.Abc as ABC
import Data.Abc.Serializer (serializeHeaders)
import Data.Abc.Parser (abcParse, headersParse)
import Data.Abc.Validator (buildHeaderMap, validateHeaders)
import qualified Data.Abc.Validator as V (ValidatedHeaders(..))
import Data.Validation (Validation(..), toEither)


-- | Newly submitted ABC metadata

data AbcMetadataSubmission = AbcMetadataSubmission
    { genreString :: Text
    , tuneId :: TuneRef.TuneId
    , submitter :: U.UserName
    , title :: Text
    , rhythm :: Text
    , key :: Text
    , abc :: Text
    , origin :: Maybe Text
    , source :: Maybe Text
    , composer :: Maybe Text
    , transcriber :: Maybe Text
    } deriving (Eq, Show, Generic)


instance ToRow AbcMetadataSubmission where
  toRow t = [ toField (genreString t), toField (tuneId t), toField (submitter t)
            , toField (title t), toField (rhythm t), toField (key t)
            , toField (abc t), toField (origin t), toField (source t)
            , toField (composer t), toField (transcriber t)
            ]

buildMetadata :: U.UserName -> Genre -> Text ->  Either String AbcMetadataSubmission
buildMetadata userName genre abcText  =
  case (abcParse abcText) of
    Left err ->
      Left err
    Right abc ->
      let
        genreStr = pack $ show genre
        headerText = serializeHeaders (ABC.headers abc)
        headerMap = buildHeaderMap $ ABC.headers abc
        validated = toEither $ validateHeaders genre headerMap
        source = lookup ABC.Source headerMap
        origin = lookup ABC.Origin headerMap
        composer = lookup ABC.Composer headerMap
        transcriber = lookup ABC.Transcription headerMap
        fromValid :: V.ValidatedHeaders -> AbcMetadataSubmission
        fromValid  (V.ValidatedHeaders title _ key rhythm ) =
          let
            tid = TuneRef.tuneId title rhythm
          in
            AbcMetadataSubmission genreStr tid userName title rhythm key
                        abcText
                        origin source composer transcriber
      in
        bimap concat fromValid validated
