{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tunebank.TypeConversion.Cache
   (removeOldFiles) where

-- | module to clear the cache of 'old' files which occur when a
-- | tune is deleted or upserted

import Prelude ()
import Prelude.Compat
import Control.Monad.Reader
import Control.Exception (try, SomeException)
import System.Directory
import System.FilePath.Posix (takeBaseName, (</>))
import Data.Genre (Genre)
import Tunebank.Config
import Tunebank.Types
import Tunebank.Model.TuneRef (TuneId, safeFileName)
import Debug.Trace (traceM)

-- | remove any source or target file from the cache which matches the tuneId
removeOldFiles :: Genre -> TuneId -> AppM ()
removeOldFiles genre tid =
  let
    fileBase = safeFileName tid
  in do
    _ <- removeMatchingSources genre fileBase
    _ <- removeMatchingBinaries genre fileBase
    pure ()

matchesName :: String-> FilePath -> Bool
matchesName target path  =
  (takeBaseName path) == target

findMatchingFiles :: FilePath -> String -> IO [FilePath]
findMatchingFiles dirPath target = do
  contents <- getDirectoryContents dirPath
  let
    matches = filter (matchesName target) contents
  _ <- traceM ("files to remove: " <> (show matches))
  pure matches

deleteFile :: FilePath -> FilePath -> IO ()
deleteFile basePath filePath = do
  eResult <- try (removeFile $ basePath </> filePath)
  case eResult of
    Left (err :: SomeException) -> do
      _ <- traceM ("Unable to delete file: " <> filePath <> " from " <> basePath <> ": " <> (show err))
      pure ()
    Right _ ->
      pure ()

removeFiles :: FilePath -> [FilePath] -> IO ()
removeFiles basePath filePaths = do
  _ <- mapM (deleteFile basePath) filePaths
  pure ()

removeMatchingBinaries :: Genre -> String -> AppM ()
removeMatchingBinaries genre target = do
  basePath <- transcodeTargetPath genre
  filePaths <- liftIO $ findMatchingFiles basePath target
  liftIO $ removeFiles basePath filePaths

removeMatchingSources :: Genre -> String -> AppM ()
removeMatchingSources genre target = do
  basePath <- transcodeSourcePath genre
  filePaths <- liftIO $ findMatchingFiles basePath target
  liftIO $ removeFiles basePath filePaths
