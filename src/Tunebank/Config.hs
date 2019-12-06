module Tunebank.Config
  ( transcodeScriptPath
  , transcodeSourcePath
  , transcodeTargetPath
  ) where

-- | Utilities for reading configuration values

import Prelude ()
import Prelude.Compat hiding (lookup)
import Control.Monad.Reader
import Data.Text (pack)
import Tunebank.Types
import Data.Configurator.Types (Config)
import Data.Configurator
import Data.Genre (Genre)
import Data.Char (toLower)

import Debug.Trace (traceM)

transcodeScriptPath :: AppM String
transcodeScriptPath =
  lookupString "tunebank.transcode.scriptDir"

transcodeSourcePath :: Genre -> AppM String
transcodeSourcePath genre = do
  path <- transcodePath genre True
  traceM ("transcode source path: " <> path)
  pure path

transcodeTargetPath :: Genre -> AppM String
transcodeTargetPath genre = do
  path <- transcodePath genre False
  traceM ("transcode target path: " <> path)
  pure path

transcodePath :: Genre -> Bool-> AppM String
transcodePath genre isSourcePath = do
  let
    dirName = if isSourcePath
        then "abc/"
        else "pdf/"
  base <- lookupString "tunebank.transcode.cacheDir"
  pure $ base <> "/core/" <> dirName  <> (map toLower $ show genre)


-- | lookup a String-valued configuration item
lookupString :: String -> AppM String
lookupString item = do
  config <- asks _getConfig
  val <- liftIO $ (require config (pack item) :: IO String)
  traceM ("looking up string: " <> item <> " - value: " <> show val)
  pure val

-- | lookup am Int-valued configuration item
lookupInt :: String -> AppM Int
lookupInt item = do
  config <- asks _getConfig
  val <- liftIO $ (require config (pack item) :: IO Int)
  traceM ("looking up int: " <> item <> " - value: " <> show val)
  pure val
