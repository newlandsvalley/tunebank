module Main where


import Test.Hspec
import UserApiTests (userApiSpec)
import TuneApiTests (tuneApiSpec)
import Data.Configurator

main :: IO ()
main = do
  config <- load [ Required "conf/tunebank-test.conf" ]
  hspec (userApiSpec config)
  hspec (tuneApiSpec config)
