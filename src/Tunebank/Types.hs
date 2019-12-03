module Tunebank.Types where


import Control.Monad.Reader
import Servant.Server (Handler)
import Data.Configurator.Types (Config)

type AppM = ReaderT AppCtx Handler

data AppCtx = AppCtx {
  _getConfig :: Config
  }
