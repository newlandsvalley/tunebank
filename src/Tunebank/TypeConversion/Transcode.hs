{-# LANGUAGE OverloadedStrings #-}

module Tunebank.TypeConversion.Transcode
  (transcodeTo) where

-- | Transcode ABC to a variety of target formats:
-- | PDF, PS, MIDI, PNG
-- | implemented by calling out to command line services
-- | which work through the file cache


import Prelude ()
import Prelude.Compat hiding (readFile)

import Control.Exception
import System.IO hiding (readFile, hGetContents)
import Tunebank.Types
import Tunebank.Model.AbcMetadata
import Control.Monad.Reader
import qualified Tunebank.Model.TuneRef as TuneRef
import Data.Map (Map, fromList, elems, lookup)
import Data.ByteString.Lazy (ByteString, readFile, hGetContents)
import Data.ByteString.Lazy.Internal (packChars)
import Data.Genre (Genre(..))
import Data.Text (Text)
import Data.Bifunctor (first)
import Tunebank.Config

data TranscodeParams = TranscodeParams
  {
    scriptName :: String
  , fileExtension :: String
  }

type MimeEntry = (Transcodable, TranscodeParams)
type MimeTargets = Map Transcodable TranscodeParams

mimeEntries :: MimeTargets
mimeEntries =
  fromList
    [ (Pdf, TranscodeParams "abc2pdf.sh" "pdf")
    , (PostScript, TranscodeParams "abc2ps.sh" "ps")
    , (Png, TranscodeParams "abc2png.sh" "png")
    , (Midi, TranscodeParams "abc2midi.sh" "midi")
    ]

transcodeTo :: Transcodable -> Genre -> AbcMetadata -> AppM (Either ByteString ByteString)
transcodeTo target genre abcMetadata = do
  let
    fileBase = TuneRef.safeFileName $ TuneRef.tuneId (title abcMetadata) (rhythm abcMetadata)
    (script, fileExtension) =
      case target of
        Pdf -> ("abc2pdf.sh", "pdf")
        PostScript -> ("abc2ps.sh", "ps")
        Png -> ("abc2png.sh", "png")
        Midi -> ("abc2midi.sh", "midi")
  -- pure $ Left "not yet implemented"
  readTargetFile genre fileBase fileExtension

readTargetFile :: Genre -> String -> String -> AppM (Either ByteString ByteString)
readTargetFile genre fileBase fileExtension = do
  basePath <- transcodeTargetPath genre
  let
    filePath = basePath <> "/" <> fileBase <> "." <> fileExtension 
  result <- liftIO $ readBinaryFile filePath
  pure $ first (const $ packChars $ "not found: " <> filePath) result

readBinaryFile :: FilePath -> IO (Either IOException ByteString)
readBinaryFile filePath = do
  result <- try $ do
    handle <- openBinaryFile filePath ReadMode
    hGetContents handle
  pure result
