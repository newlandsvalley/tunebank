
{-# LANGUAGE OverloadedStrings #-}


module TranscodingTests (transcodingSpec) where

import Prelude ()
import Prelude.Compat

import Control.Exception (bracket)
import Data.Text (Text, pack, unpack)
import Data.Aeson
import Data.Genre (Genre(..))
import Data.Time.Calendar
import Data.Time.Clock (UTCTime)
import Tunebank.Utils.Timestamps (fromDay)

import Test.Hspec

import Tunebank.Types
import Tunebank.Model.User
import Tunebank.Model.TuneText
import Tunebank.Model.TuneRef
import Tunebank.Model.AbcMetadata
import TestData

transcodingSpec :: Spec
transcodingSpec =

  describe "ABCMetadata" $ do
    it "should be round-trippable to itself through JSON" $ do
      let
        time = fromDay $ fromGregorian 2020 1 1
        eMetadata = buildMetadata (UserName "fred") time Scandi augustsson
      case eMetadata of
        Left err ->
          expectationFailure "unexpected build metadata error"
        Right metadata ->
          (fromJSON $ toJSON metadata)  `shouldBe` (Success metadata)
