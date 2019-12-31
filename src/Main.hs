module Main where

import Tunebank.Types (AppCtx(..))
import Tunebank.Server (fullApp)
import Network.Wai.Handler.Warp (run)
import Data.Configurator
import Tunebank.Config (getPort)

main :: IO ()
main = do
  config <- load [ Required "conf/tunebank.conf" ]
  port <- getPort config
  run port (fullApp $ AppCtx config)
