module Main where

import Tunebank.Server (app1, app2, app3)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = run 8081 app3
