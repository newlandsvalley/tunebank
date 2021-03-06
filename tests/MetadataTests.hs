
{-# LANGUAGE OverloadedStrings #-}

module MetadataTests (metadataSpec) where

import Prelude ()
import Prelude.Compat

import Data.Aeson
import Data.Genre (Genre(..))
import Test.Hspec

import Tunebank.Model.User
import Tunebank.Model.AbcMetadata
import TestData

metadataSpec :: Spec
metadataSpec =

  describe "ABCMetadata" $ do
    it "should be round-trippable to itself through JSON" $ do
      let
        eMetadata = buildClientMetadata (UserName "fred") "09 Feb 2020" Scandi augustsson
      case eMetadata of
        Left err ->
          expectationFailure "unexpected build metadata error"
        Right metadata ->
          (fromJSON $ toJSON metadata)  `shouldBe` (Success metadata)
