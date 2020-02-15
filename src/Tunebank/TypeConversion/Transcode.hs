{-# LANGUAGE OverloadedStrings #-}

module Tunebank.TypeConversion.Transcode
  ( transcodeTo ) where

-- | Transcode ABC to a variety of target formats:
-- | PDF, PS, MIDI, PNG
-- | implemented by calling out to command line services
-- | which work through the file cache

-- | ByteStrings required by Servant are Lazy.  However, Lazy File IO is bug-ridden
-- | and so we use strict ByteStrings in file IO and convert using fromStrict
-- | wherever necessary


import Prelude ()
import Prelude.Compat hiding (readFile)

import Control.Exception
import System.IO hiding (readFile, hGetContents)
import System.Exit (ExitCode(..))
import System.Directory (doesFileExist)
import System.Process
import Tunebank.Types
import Tunebank.Model.AbcMetadata
import Control.Monad.Reader
import qualified Tunebank.Model.TuneRef as TuneRef
import qualified Data.ByteString as Strict (ByteString, hGetContents)
import Data.ByteString.Lazy (ByteString, fromStrict)
import Data.ByteString.Lazy.Internal (packChars)
import Data.Genre (Genre(..))
import Data.Text (Text, unpack)
import Data.Maybe (Maybe)
import Data.Bifunctor (first)
import Tunebank.Config

import Debug.Trace (traceM)

transcodeTo :: Transcodable -> Genre -> AbcMetadata -> AppM (Either ByteString ByteString)
transcodeTo target genre abcMetadata = do
  let
    fileBase = TuneRef.safeFileName $ TuneRef.tuneId (title abcMetadata) (rhythm abcMetadata)
    (scriptName, fileExtension) =
      case target of
        Pdf -> ("abc2pdf.sh", "pdf")
        PostScript -> ("abc2ps.sh", "ps")
        Png -> ("abc2png.sh", "png")
        Midi -> ("abc2midi.sh", "midi")
  targetFilePath <- buildTargetFilePath genre fileBase fileExtension
  exists <- liftIO $ doesFileExist targetFilePath
  if exists
    then readTargetFile targetFilePath
    else do
      sourceDir <- transcodeSourcePath genre
      targetDir <- transcodeTargetPath genre
      scriptDir <- transcodeScriptPath
      sourceFilePath <- buildSourceFilePath genre fileBase
      let
        script = scriptDir <> "/" <> scriptName
        -- abcText = (abcHeaders abcMetadata) <> (abcBody abcMetadata)
        abcText = (abc abcMetadata)
      _ <- liftIO $ writeTextFile sourceFilePath abcText
      transcodeError <- liftIO $ runTranscodeScript script sourceDir targetDir fileBase
      case transcodeError of
        Just err -> pure $ Left (fromStrict err)
        Nothing -> do
          readTargetFile targetFilePath

runTranscodeScript :: String -> String -> String -> String -> IO (Maybe Strict.ByteString)
runTranscodeScript script sourcePath targetPath name = do
  (_, _, herr, hp) <-
      createProcess (proc script [sourcePath, targetPath, name]){ std_err = CreatePipe }
  exitCode <- waitForProcess hp
  traceM ("transcode exit code: " <> (show exitCode))
  case exitCode of
    ExitSuccess -> do
      let
        _ = fmap hClose herr
      pure Nothing
    _ -> do
      let
        sErr = sequence $ fmap Strict.hGetContents herr
        _ = fmap hClose herr
      sErr

readTargetFile :: FilePath -> AppM (Either ByteString ByteString)
readTargetFile filePath = do
  result <- liftIO $ readBinaryFile filePath
  pure $ first (const $ packChars $ "not found: " <> filePath) result

buildSourceFilePath :: Genre -> String -> AppM FilePath
buildSourceFilePath genre fileBase  = do
  basePath <- transcodeSourcePath genre
  pure $ basePath <> "/" <> fileBase <> "." <> "abc"

buildTargetFilePath :: Genre -> String -> String -> AppM FilePath
buildTargetFilePath genre fileBase fileExtension = do
  basePath <- transcodeTargetPath genre
  pure $ basePath <> "/" <> fileBase <> "." <> fileExtension

-- | read a binary file which has been created as the result of transcoding
readBinaryFile :: FilePath -> IO (Either IOException ByteString)
readBinaryFile filePath = do
  result <- try $ do
    hndl <- openBinaryFile filePath ReadMode
    contents <- Strict.hGetContents hndl
    --  _ <- hClose hndl
    pure $ fromStrict contents
  pure result

-- | write the ABC as a text file as a source for the transcoding
writeTextFile :: FilePath -> Text -> IO (Either IOException ())
writeTextFile filePath abcText = do
  let
    contents = unpack abcText
  result <- try $ do
    hndl <- openFile filePath WriteMode
    _ <- hPutStr hndl contents
    hClose hndl
  pure result
