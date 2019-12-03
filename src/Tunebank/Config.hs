module Tunebank.Config where

-- | Utilities for reading configuration values

import Prelude ()
import Prelude.Compat hiding (lookup)
import Control.Monad.Reader
import Data.Text (pack)
import Tunebank.Types
import Data.Configurator.Types (Config)
import Data.Configurator

import Debug.Trace (traceM)


-- | lookup a String-valued configuration item
lookupString :: String -> AppM (Maybe String)
lookupString item = do
  config <- asks _getConfig
  mval <- liftIO $ (lookup config (pack item) :: IO (Maybe String))
  traceM ("looking up string: " <> item <> " - value: " <> show mval)
  pure mval

-- | lookup am Int-valued configuration item
lookupInt :: String -> AppM (Maybe Int)
lookupInt item = do
    config <- asks _getConfig
    mval <- liftIO $ (lookup config (pack item) :: IO (Maybe Int))
    traceM ("looking up int: " <> item <> " - value: " <> show mval)
    pure mval
