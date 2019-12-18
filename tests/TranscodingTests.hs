
{-# LANGUAGE OverloadedStrings #-}


module TranscodingTests (transcodingSpec) where

import Prelude ()
import Prelude.Compat

import Control.Exception (bracket)
import Data.Text (Text, pack, unpack)
import Data.Aeson
import Data.Genre (Genre(..))

import Test.Hspec

import Tunebank.Types
import Tunebank.Server (tuneApp)
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
        eMetadata = buildMetadata (UserName "fred") Scandi augustsson
      case eMetadata of
        Left err ->
          expectationFailure "unexpected build metadata error"
        Right metadata ->
          (fromJSON $ toJSON metadata)  `shouldBe` (Success metadata)
