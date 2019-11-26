module Main where

import Tunebank.Server (app1, app2)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = run 8081 app2
