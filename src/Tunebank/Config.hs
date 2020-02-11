module Tunebank.Config
  ( getPort
  , thisServerBaseUrl
  , transcodeScriptPath
  , transcodeSourcePath
  , transcodeTargetPath
  , getPageSize
  , mailHost
  , mailPort
  , mailLogin
  , mailPassword
  , mailFromAddress
  , getDbConnectInfo
  , getPoolStripes
  , getPoolConnectionsPerStripe
  , getPoolKeepOpenTime
  ) where

-- | Utilities for reading configuration values

import Prelude ()
import Prelude.Compat hiding (lookup)
import Control.Monad.Reader
import Network.Socket (PortNumber)
import Data.Text (Text, pack)
import Data.Time.Clock (NominalDiffTime)
import Tunebank.Types
import Data.Configurator.Types (Config)
import Data.Configurator
import Data.Genre (Genre)
import Data.Char (toLower)
import Unsafe.Coerce
import Database.PostgreSQL.Simple (ConnectInfo(..), defaultConnectInfo)

import Debug.Trace (traceM)

thisServerBaseUrl :: AppM String
thisServerBaseUrl = do
  host <- lookupString "tunebank.server.host"
  port <- lookupInt "tunebank.server.port"
  pure $ "http://" <> host <>":" <> (show port)

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

getPageSize :: Maybe Int -> AppM Int
getPageSize mSize = do
  defaultSize <- lookupInt "tunebank.paging.defaultSize"
  case mSize of
    Nothing -> pure defaultSize
    Just size -> do
      if (size < 5 ||  size > 100)
        then pure defaultSize
        else pure size

mailHost :: AppM String
mailHost =
  lookupString "tunebank.mail.host"

mailPort :: AppM PortNumber
mailPort = do
  portNumber <- lookupInt "tunebank.mail.port"
  pure $ unsafeCoerce portNumber

mailFromAddress :: AppM Text
mailFromAddress = do
  addr <- lookupString "tunebank.mail.fromAddress"
  pure $ pack addr

mailLogin :: AppM String
mailLogin = do
  lookupString "tunebank.mail.login"

mailPassword :: AppM String
mailPassword = do
  lookupString "tunebank.mail.password"

getDbConnectInfo :: Config -> IO ConnectInfo
getDbConnectInfo config = do
  database <- getString config "tunebank.database.dbName"
  user <- getString config "tunebank.database.user"
  password <- getString config "tunebank.database.password"
  let
    connectInfo =
      defaultConnectInfo { connectDatabase = database
                         , connectUser = user
                         , connectPassword = password
                         }
  pure connectInfo

getPoolStripes :: Config -> IO Int
getPoolStripes config =
  getInt config "tunebank.database.stripes"

getPoolConnectionsPerStripe :: Config -> IO Int
getPoolConnectionsPerStripe config =
  getInt config "tunebank.database.connectionsPerStripe"

getPoolKeepOpenTime :: Config -> IO NominalDiffTime
getPoolKeepOpenTime config = do
  secs <- getInt config "tunebank.database.keepOpenTime"
  pure ((fromIntegral secs) :: NominalDiffTime)

getPort :: Config -> IO Int
getPort config =
  getInt config "tunebank.server.port"
    -- (require config (pack "tunebank.server.port") :: IO Int)

-- | lookup a String-valued configuration item
lookupString :: String -> AppM String
lookupString item = do
  config <- asks _getConfig
  val <- liftIO $ (require config (pack item) :: IO String)
  -- traceM ("looking up string: " <> item <> " - value: " <> show val)
  pure val

-- | lookup am Int-valued configuration item
lookupInt :: String -> AppM Int
lookupInt item = do
  config <- asks _getConfig
  val <- liftIO $ (require config (pack item) :: IO Int)
  -- traceM ("looking up int: " <> item <> " - value: " <> show val)
  pure val


getString :: Config -> String -> IO String
getString config name =
  (require config (pack name) :: IO String)

getInt :: Config -> String -> IO Int
getInt config name =
  (require config (pack name) :: IO Int)
