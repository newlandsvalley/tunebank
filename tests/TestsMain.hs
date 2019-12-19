module Main where


import Test.Hspec
import UserApiTests (userApiSpec)
import TuneApiTests (tuneApiSpec)
import CommentApiTests (commentApiSpec)
import TranscodingTests (transcodingSpec)
import Data.Configurator

main :: IO ()
main = do
  config <- load [ Required "conf/tunebank-test.conf" ]
  hspec (userApiSpec config)
  hspec (tuneApiSpec config)
  hspec (commentApiSpec config)
  hspec transcodingSpec
