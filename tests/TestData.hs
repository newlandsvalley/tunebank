{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module TestData where

import Prelude ()
import Prelude.Compat
import Data.Text (Text)
import Servant
import Tunebank.Model.User
import Tunebank.Model.TuneRef
import qualified Tunebank.Model.UserRegistration as UReg (Submission(..))


-- users
sampleNewUser :: UReg.Submission
sampleNewUser = UReg.Submission "fred" "fred@gmail.com" "pwd" "pwd" "http://localhost"

admin :: BasicAuthData
admin =
  BasicAuthData "Administrator" "password"

normalUser :: BasicAuthData
normalUser =
  BasicAuthData "Fred" "password"

badUser :: BasicAuthData
badUser =
  BasicAuthData "Joe" "wibble"

validateableUid :: UserId
validateableUid =
  UserId "FRED"


-- tunes
badAbc :: String
badAbc =
  "foo\r\n"

augustsson :: String
augustsson =
  "X:1\r\n"
  <> "T:Engelska efter Albert Augustsson\r\n"
  <> "N:From the playing of Albert Augustsson, Bohuslän county.\r\n"
  <> "M:4/4\r\n"
  <> "R:Engelska\r\n"
  <> "S:Orust\r\n"
  <> "Z:John Watson 24/01/2015\r\n"
  <> "L:1/8\r\n"
  <> "K:A\r\n"
  <> "A>c|: e2f2 efed | c2a2 e3d | cedc BdcB | Aced cBAc |\r\n"
  <> "e2f2 efed | c2a2 e3d | cedc BdcB | A4 A>AA>B :|\r\n"
  <> "|: e2e2 e2de | f2ed B3c | d3c d2cd | e3d cdBc |\r\n"
  <> "A2a2 a2gf | e2f2 e3d | cedc BdcB |1 A4 A>AA>B :|2 [A4E4] [A4E4] |\r\n"

augustssonId :: TuneId
augustssonId =
  TuneId "engelska efter albert augustsson-engelska"


norefno :: String
norefno =
  "T: Fastan\r\n"
  <> "R: Polska\r\n"
  <> "M: 3/4\r\n"
  <> "K: F\r\n"
  <> "L: 1/16\r\n"
  <> "| (3A4F4G4 A2B2 | (3:4:3c2d2B4c4 A2F2 | (3F4E4D4 B,2D2 | EA3 A8- |\r\n"

notitle :: String
notitle =
  "X:1\r\n"
  <> "R: Polska\r\n"
  <> "M: 3/4\r\n"
  <> "K: F\r\n"
  <> "L: 1/16\r\n"
  <> "| (3A4F4G4 A2B2 | (3:4:3c2d2B4c4 A2F2 | (3F4E4D4 B,2D2 | EA3 A8- |\r\n"

badrhythm :: String
badrhythm =
  "X:1\r\n"
  <>  "T: Fastan\r\n"
  <> "R: Reel\r\n"
  <> "M: 3/4\r\n"
  <> "K: F\r\n"
  <> "L: 1/16\r\n"
  <> "| (3A4F4G4 A2B2 | (3:4:3c2d2B4c4 A2F2 | (3F4E4D4 B,2D2 | EA3 A8- |\r\n"
