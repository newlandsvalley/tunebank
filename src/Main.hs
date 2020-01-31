module Main where

import Tunebank.Types (AppCtx(..))
import Tunebank.Server (fullApp)
import Network.Wai.Handler.Warp (run)
import Data.Configurator
import Tunebank.Config (getPort, getDbConnectInfo, getPoolStripes,
                       getPoolConnectionsPerStripe, getPoolKeepOpenTime)
import Tunebank.DB.Api (DBConfig(..))
import Database.PostgreSQL.Simple
import Data.Pool

main :: IO ()
main = do
  config <- load [ Required "conf/tunebank.conf" ]
  port <- getPort config
  -- database connection configuration
  dbConnectInfo <- getDbConnectInfo config
  -- database resource pool configuration
  poolStripes <- getPoolStripes config
  connectionsPerStripe <- getPoolConnectionsPerStripe config
  keepOpenTime <- getPoolKeepOpenTime config
  pool <- createPool (connect dbConnectInfo)
             close
             poolStripes          -- stripes  (2)
             keepOpenTime         -- unused connections are kept open for...(60 sec)
             connectionsPerStripe -- max. connections open per stripe (10)
  run port (fullApp (AppCtx config) (DBConfig pool))
