{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module TestData where

import Prelude ()
import Prelude.Compat
import Data.Text (Text)
import Data.Time.Calendar
import Servant
import Tunebank.Model.User
import Tunebank.Model.TuneRef
import Tunebank.Model.Comment
import Tunebank.Utils.Timestamps
import qualified Tunebank.Model.UserRegistration as UReg (Submission(..))
import qualified Tunebank.Model.CommentSubmission as NewComment


-- users
sampleNewUser :: UReg.Submission
sampleNewUser = UReg.Submission "fred" "fred@gmail.com" "pwd" "pwd" "http://localhost"

admin :: BasicAuthData
admin =
  BasicAuthData "administrator" "password"

normalUser :: BasicAuthData
normalUser =
  BasicAuthData "Fred" "password"

badUser :: BasicAuthData
badUser =
  BasicAuthData "Joe" "wibble"

validateableUid :: UserId
validateableUid =
  UserId 1

sampleCommentId :: CommentId
sampleCommentId =
  CommentId $ day2timestamp $ (fromGregorian 2019  12 14)

sampleNewComment :: NewComment.Submission
sampleNewComment = NewComment.Submission (CommentId "cid") "user" "subject" "text"

-- tunes
badAbc :: Text
badAbc =
  "foo\r\n"

augustsson :: Text
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

andetBrudstykke :: Text
andetBrudstykke =
  "X:1\r\n"
  <> "T:Andet brudestykke\r\n"
  <> "C:Sonderho, Denmark\r\n"
  <> "S:from Sonderho, Denmark\r\n"
  <> "Z:Peter Uhrbrand\r\n"
  <> "M:2/4\r\n"
  <> "L:1/16\r\n"
  <> "R:marsch\r\n"
  <> "K:Dmaj\r\n"
  <> "| A2 |: d2d2 B2Bd | c2c2 -c2Bc | d2B2 B2Bc | B2A2 A4 :|\r\n"
  <> "d2d2 e2f2 | g3f -f2ed | c2d2 e2f2 | g3f -f2ed | c2A2 ABc2 |\r\n"
  <> "d2d2 A2AB | A2d2 e2f2 | g3f -f2ed | c2d2 e2f2 | g3f -f2ed |\r\n"
  <> "c2A2 ABc2 !D.C.! y || !coda! y [| d2d2 d4 |]\r\n"

andetBrudstykkeId :: TuneId
andetBrudstykkeId =
  TuneId "andet brudestykke-marsch"


fastan :: Text
fastan =
  "X:1\r\n"
  <> "T: Fastan\r\n"
  <> "R: Polska\r\n"
  <> "M: 3/4\r\n"
  <> "K: F\r\n"
  <> "L: 1/16\r\n"
  <> "| (3A4F4G4 A2B2 | (3:4:3c2d2B4c4 A2F2 | (3F4E4D4 B,2D2 | EA3 A8- |\r\n"
  <> "| (3A4F4G4 A2B2 | (3:4:3c2d2B4c4 A2F2 | (3F4E4D4 G2A2 | AF3 F8- |\r\n"
  <> "| (3:5:3F4B4cBA2 B2d2 | ge3 c4 A4- | (3:5:3A4B4cBA2 B2d2 | de3 c8 |\r\n"
  <> "| (3:5:3F4B4cBA2 B2d2 | (3:4:3g2a2f4g4 e4 | (3:c4B4A4 F2G2 | ef3 F8 |\r\n"

fastanId :: TuneId
fastanId =
  TuneId "fastan-polska"

cig :: Text
cig =
  "X: 1\r\n"
  <> "T: C i G, Grind Hans Jässpôdspolska\r\n"
  <> "Z: Christine Dyer\r\n"
  <> "R: Polska\r\n"
  <> "O: Rättvik, Dalarna\r\n"
  <> "M: 3/4\r\n"
  <> "L: 1/8\r\n"
  <> "K:Gmaj\r\n"
  <> "|: D2 G2 A>B | (3c2B2G2 E2- | E2 c>B A>G | F/2G/2A- AF D2- |\r\n"
  <> "D2 G2 A>B | (3c2B2G2 E2- | E2 c>B AG |1 (3FDF G4 :|2  (3FDF G3 D |\r\n"
  <> "|: G>B dc B>A | G<c ed c>B | A>B c/2d/2c/2A/2 B>G |F/2G/2A- AF D>D |\r\n"
  <> "G>B dc B>A | G<c ed c>B | A>B c/2d/2c/2A/2 B>G |1 FD/2F/2 G3 D :|2 FD/2F/2 G4 |\r\n"
  <> "|  |\r\n"

cigId :: TuneId
cigId =
  TuneId "c i g, grind hans jässpôdspolskan-polska"

norefno :: Text
norefno =
  "T: Fastan\r\n"
  <> "R: Polska\r\n"
  <> "M: 3/4\r\n"
  <> "K: F\r\n"
  <> "L: 1/16\r\n"
  <> "| (3A4F4G4 A2B2 | (3:4:3c2d2B4c4 A2F2 | (3F4E4D4 B,2D2 | EA3 A8- |\r\n"

notitle :: Text
notitle =
  "X:1\r\n"
  <> "R: Polska\r\n"
  <> "M: 3/4\r\n"
  <> "K: F\r\n"
  <> "L: 1/16\r\n"
  <> "| (3A4F4G4 A2B2 | (3:4:3c2d2B4c4 A2F2 | (3F4E4D4 B,2D2 | EA3 A8- |\r\n"

badrhythm :: Text
badrhythm =
  "X:1\r\n"
  <>  "T: Fastan\r\n"
  <> "R: Reel\r\n"
  <> "M: 3/4\r\n"
  <> "K: F\r\n"
  <> "L: 1/16\r\n"
  <> "| (3A4F4G4 A2B2 | (3:4:3c2d2B4c4 A2F2 | (3F4E4D4 B,2D2 | EA3 A8- |\r\n"
