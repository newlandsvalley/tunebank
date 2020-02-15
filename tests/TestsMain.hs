module Main where


import Test.Hspec
import UserApiTests (userApiSpec)
import TuneApiTests (tuneApiSpec)
import CommentApiTests (commentApiSpec)
import MetadataTests (metadataSpec)
import Data.Configurator

main :: IO ()
main = do
  config <- load [ Required "conf/tunebank-tests.conf" ]
  hspec (userApiSpec config)
  hspec (tuneApiSpec config)
  hspec (commentApiSpec config)
  hspec metadataSpec
