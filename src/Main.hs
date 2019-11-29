module Main where

import Tunebank.Server (userApp, tuneApp, commentApp)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = run 8081 userApp
