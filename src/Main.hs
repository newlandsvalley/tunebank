module Main where

import Tunebank.Types (AppCtx(..))
import Tunebank.Server (userApp, tuneApp, commentApp)
import Network.Wai.Handler.Warp (run)
import Data.Configurator

main :: IO ()
main = do
  config <- load [ Required "conf/tunebank.conf" ]
  run 8081 (tuneApp $ AppCtx config)
