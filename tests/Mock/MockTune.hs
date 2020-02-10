{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Mock.MockTune
  ( findTuneById
  , getTuneList
  , search
  , countTunes
  , insertTune
  ) where


import Prelude ()
import Prelude.Compat hiding (lookup)
import Data.Map (Map, fromList, elems, lookup)
import Data.Text (Text, pack, unpack, toLower)
import Data.Maybe (catMaybes, fromJust)
import Data.Either (Either(..))
import Data.Time.Calendar
import Data.Time.Clock (UTCTime)
import Tunebank.Utils.Timestamps (fromDay)
import Data.Tuple (fst)
import Control.Error.Util (hush)

import Data.Genre (Genre(..))
import Tunebank.Model.AbcMetadata
import qualified Tunebank.Model.AbcMetadataSubmission as NewTune (AbcMetadataSubmission(..))
import qualified Tunebank.Model.TuneText as S (Submission(..))
import qualified Tunebank.Model.TuneRef as TuneRef
import Tunebank.Model.Pagination
import Tunebank.Types
import Tunebank.Model.User (UserName(..))
import TestData (augustsson, fastan, cig, andetBrudstykke)

import Debug.Trace (trace)

type MetadataEntry = (TuneRef.TuneId, AbcMetadata)

findTuneById :: Genre -> TuneRef.TuneId -> Maybe AbcMetadata
findTuneById genre tuneId =
  case genre of
    Scandi ->
      let
        tracedTuneId = trace ("scandi tuneId: " <> (show tuneId)) tuneId
      in
        lookup tracedTuneId scandiMetadata
    _ ->
      Nothing


getTuneList :: Genre -> [TuneRef.TuneRef]
getTuneList genre =
  case genre of
    Scandi ->
      map buildTuneRef $ elems scandiMetadata
    _ ->
      []


buildTuneRef :: AbcMetadata -> TuneRef.TuneRef
buildTuneRef metadata =
  TuneRef.TuneRef
    { TuneRef.uri = TuneRef.tuneId (title metadata) (rhythm metadata)
    , TuneRef.title = title metadata
    , TuneRef.rhythm = rhythm metadata
    , TuneRef.abc = abc metadata
    , TuneRef.date = "04 Feb 2020"
    }


buildMetadataEntry :: UserName -> Text -> Genre -> Text -> Either String MetadataEntry
buildMetadataEntry userName dateString genre abcText =
  case (buildClientMetadata userName dateString genre abcText) of
    Left err ->
      Left err
    Right metadata ->
      let
        tuneKey = TuneRef.tuneId (title metadata) (rhythm metadata)
      in
        Right (tuneKey, metadata)

scandiMetadata :: Map TuneRef.TuneId AbcMetadata
scandiMetadata =
  let
    submitter = UserName (pack "Administrator")
  in
    fromList $ catMaybes $ map (hush . buildMetadataEntry submitter "09 Feb 2020" Scandi) scandiAbc

scandiAbc :: [ Text ]
scandiAbc =
  [augustsson, fastan, cig, andetBrudstykke]


search :: Genre
       -> Maybe Title
       -> Maybe Rhythm
       -> Maybe TuneKey
       -> Maybe Source
       -> Maybe Origin
       -> Maybe Composer
       -> Maybe Transcriber
       -> SortKey
       -> Int
       -> Int
       -> [TuneRef.TuneRef]
search genre mTitle mRhythm mKey mSource mOrigin
           mComposer mTranscriber sortKey limit offset =
    let
      -- we'll get rid of this logging quite soon
      !p1 = trace ("title param: " <> show mTitle) mTitle
      !p2 = trace ("rhythm param: " <> show mRhythm) mRhythm
      !p3 = trace ("key param: " <> show mKey) mKey
      !p4 = trace ("source param: " <> show mSource) mSource
      !p5 = trace ("origin param: " <> show mOrigin) mOrigin
      !p6 = trace ("composer param: " <> show mComposer) mComposer
      !p7 = trace ("transcriber param: " <> show mTranscriber) mTranscriber
      !p8 = trace ("sort param: " <> show sortKey) sortKey
      {-}
      count = countTunes genre mTitle mRhythm mKey mSource mOrigin mComposer mTranscriber
      maxPages = (count + limit - 1) `quot` limit
      pagination = Pagination page limit maxPages
      -}
    in
      getTuneList genre

countTunes :: Genre
           -> Maybe Title
           -> Maybe Rhythm
           -> Maybe TuneKey
           -> Maybe Source
           -> Maybe Origin
           -> Maybe Composer
           -> Maybe Transcriber
           -> Int
countTunes genre mTitle mRhythm mKey mSource mOrigin mComposer mTranscriber =
  length $ getTuneList genre

insertTune :: NewTune.AbcMetadataSubmission -> TuneRef.TuneId
insertTune metadata =
  -- assume the insert works, but we need to check the return
  let
    tid = NewTune.tuneId metadata
    foo = trace ("Mock insert tune for " <> show tid)
  in
    tid
