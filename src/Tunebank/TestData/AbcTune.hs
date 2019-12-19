{-# LANGUAGE BangPatterns #-}

module Tunebank.TestData.AbcTune
  (
    getTuneMetadata
  , search
  , getTuneList
  , postNewTune
  , getTuneBinary
  , deleteTune
  ) where

import Prelude ()
import Prelude.Compat hiding (lookup)

import System.IO (FilePath)
import Data.Map (Map, fromList, elems, lookup)
import Data.Text (Text, pack, unpack, toLower)
import Data.Maybe (catMaybes, fromJust)
import Data.Either (Either(..))
import Data.Time.Calendar
import Data.Tuple (fst)
import Control.Error.Util (hush)


import Data.ByteString.Lazy.Internal (ByteString, packChars)
import Data.Genre
import Tunebank.Model.AbcMetadata
import qualified Tunebank.Model.TuneText as S (Submission(..))
import qualified Tunebank.Model.TuneRef as TuneRef
import Tunebank.Model.Pagination
import Tunebank.Types
import Tunebank.Model.User (UserName(..))
import Tunebank.TypeConversion.Transcode (transcodeTo)
import Tunebank.TestData.User (hasDeletePermission)

import Debug.Trace (trace)

type MetadataEntry = (TuneRef.TuneId, AbcMetadata)

buildMetadataEntry :: UserName -> Genre -> Text -> Either String MetadataEntry
buildMetadataEntry userName genre abcText =
  case (buildMetadata userName genre abcText) of
    Left err ->
      Left err
    Right metadata ->
      let
        tuneKey = TuneRef.tuneId (title metadata) (rhythm metadata)
      in
        Right (tuneKey, metadata)

buildTuneRef :: AbcMetadata -> TuneRef.TuneRef
buildTuneRef metadata =
  TuneRef.TuneRef
    { TuneRef.uri = TuneRef.tuneId (title metadata) (rhythm metadata)
    , TuneRef.title = title metadata
    , TuneRef.rhythm = rhythm metadata
    , TuneRef.abcHeaders = abcHeaders metadata
    , TuneRef.abc = abcBody metadata
    , TuneRef.ts = (fromGregorian 1683  3 1)
    }

-- at the moment, we don't properly apply search parameters - just log them
search :: Genre
       -> Maybe Title
       -> Maybe Rhythm
       -> Maybe TuneKey
       -> Maybe Source
       -> Maybe Origin
       -> Maybe Composer
       -> Maybe Transcriber
       -> Maybe SortKey
       -> Int
       -> Int
       -> TuneRef.TuneList
search genre mTitle mRhythm mKey mSource mOrigin
         mComposer mTranscriber mSortKey page size =
  let
    -- we'll get rid of this logging quite soon
    !p1 = trace ("title param: " <> show mTitle) mTitle
    !p2 = trace ("rhythm param: " <> show mRhythm) mRhythm
    !p3 = trace ("key param: " <> show mKey) mKey
    !p4 = trace ("source param: " <> show mSource) mSource
    !p5 = trace ("origin param: " <> show mOrigin) mOrigin
    !p6 = trace ("composer param: " <> show mComposer) mComposer
    !p7 = trace ("transcriber param: " <> show mTranscriber) mTranscriber
    !p8 = trace ("sort param: " <> show mSortKey) mSortKey
    count = countTunes genre mTitle mRhythm mKey mSource mOrigin mComposer mTranscriber
    maxPages = (count + size - 1) `quot` size
    pagination = Pagination page size maxPages
  in
    TuneRef.TuneList (getTuneList genre) pagination

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


getTuneList :: Genre -> [TuneRef.TuneRef]
getTuneList genre =
  case genre of
    Scandi ->
      map buildTuneRef $ elems scandiMetadata
    _ ->
      []

postNewTune :: UserName -> Genre -> S.Submission -> Either ByteString TuneRef.TuneId
postNewTune userName genre submission =
  let
    theText = S.abc submission
    tracedText = trace ("new tune text: " <> (show theText)) theText
  in
    case (buildMetadataEntry userName genre $ S.abc submission) of
      Left err ->
        Left $ packChars err
      Right metadata ->
        Right (fst metadata)

deleteTune :: UserName -> Genre -> TuneRef.TuneId -> Either ByteString ()
deleteTune userName genre tuneId =
  case (getTuneMetadata genre tuneId) of
    Nothing ->
      Left $ packChars ("tune name not recognized")
    Just metadata -> do
      if (hasDeletePermission userName (submitter metadata))
        then
          Right ()
        else
          Left $ packChars ("permission denied")

getTuneMetadata :: Genre -> TuneRef.TuneId -> Maybe AbcMetadata
getTuneMetadata genre tuneId =
  case genre of
    Scandi ->
      let
        tracedTuneId = trace ("scandi tuneId: " <> (show tuneId)) tuneId
      in
        lookup tracedTuneId scandiMetadata
    _ ->
      Nothing

getTuneBinary :: Transcodable -> Genre -> TuneRef.TuneId -> AppM (Either ByteString ByteString)
getTuneBinary binaryFormat genre tuneId =
  case (getTuneMetadata genre tuneId) of
    Nothing ->
      pure $ Left $ packChars ("tune name not recognized")
    Just metadata ->
      transcodeTo binaryFormat genre metadata

scandiAbc :: [ Text ]
scandiAbc =
  [(pack augustsson), (pack fastan), (pack cig)]

scandiMetadata :: Map TuneRef.TuneId AbcMetadata
scandiMetadata =
  let
    submitter = UserName (pack "Administrator")
  in
    fromList $ catMaybes $ map (hush . buildMetadataEntry submitter Scandi) scandiAbc


augustsson :: String
augustsson =
  "X:1\r\n"
  <> "T:Engelska efter Albert Augustsson\r\n"
  <> "N:From the playing of Albert Augustsson, Bohuslän county.\r\n"
  <> "M:4/4\r\n"
  <> "R:Engelska\r\n"
  <> "S:Orust\r\n"
  <> "Z:John Watson 24/01/2015\r\n"
  <> "L:1/8\r\n"
  <> "K:A\r\n"
  <> "A>c|: e2f2 efed | c2a2 e3d | cedc BdcB | Aced cBAc |\r\n"
  <> "e2f2 efed | c2a2 e3d | cedc BdcB | A4 A>AA>B :|\r\n"
  <> "|: e2e2 e2de | f2ed B3c | d3c d2cd | e3d cdBc |\r\n"
  <> "A2a2 a2gf | e2f2 e3d | cedc BdcB |1 A4 A>AA>B :|2 [A4E4] [A4E4] |\r\n"

fastan :: String
fastan =
  "X:1\r\n"
  <> "T: Fastan\r\n"
  <> "R: Polska\r\n"
  <> "M: 3/4\r\n"
  <> "K: F\r\n"
  <> "L: 1/16\r\n"
  <> "| (3A4F4G4 A2B2 | (3:4:3c2d2B4c4 A2F2 | (3F4E4D4 B,2D2 | EA3 A8- |\r\n"
  <> "| (3A4F4G4 A2B2 | (3:4:3c2d2B4c4 A2F2 | (3F4E4D4 G2A2 | AF3 F8- |\r\n"
  <> "| (3:5:3F4B4cBA2 B2d2 | ge3 c4 A4- | (3:5:3A4B4cBA2 B2d2 | de3 c8 |\r\n"
  <> "| (3:5:3F4B4cBA2 B2d2 | (3:4:3g2a2f4g4 e4 | (3:c4B4A4 F2G2 | ef3 F8 |\r\n"

cig :: String
cig =
  "X: 1\r\n"
  <> "T: C i G, Grind Hans Jässpôdspolska\r\n"
  <> "Z: Christine Dyer\r\n"
  <> "R: Polska\r\n"
  <> "O: Rättvik, Dalarna\r\n"
  <> "M: 3/4\r\n"
  <> "L: 1/8\r\n"
  <> "K:Gmaj\r\n"
  <> "|: D2 G2 A>B | (3c2B2G2 E2- | E2 c>B A>G | F/2G/2A- AF D2- |\r\n"
  <> "D2 G2 A>B | (3c2B2G2 E2- | E2 c>B AG |1 (3FDF G4 :|2  (3FDF G3 D |\r\n"
  <> "|: G>B dc B>A | G<c ed c>B | A>B c/2d/2c/2A/2 B>G |F/2G/2A- AF D>D |\r\n"
  <> "G>B dc B>A | G<c ed c>B | A>B c/2d/2c/2A/2 B>G |1 FD/2F/2 G3 D :|2 FD/2F/2 G4 |\r\n"
  <> "|  |\r\n"
