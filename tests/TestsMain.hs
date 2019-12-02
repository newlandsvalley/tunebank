module Main where

import Test.Hspec
import UserApiTests (userApiSpec)
import TuneApiTests (tuneApiSpec)

main :: IO ()
main = do
  hspec userApiSpec
  hspec tuneApiSpec
