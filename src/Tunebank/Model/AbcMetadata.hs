
{-# LANGUAGE DeriveGeneric #-}

module Tunebank.Model.AbcMetadata where

import Data.Time.Calendar
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Aeson.Parser
import Data.Text (Text, pack, toLower)
import Data.Maybe (Maybe)

-- | a tune represented in ABC notation with the important header metadata
-- | made more easily searchable
data AbcMetadata = AbcMetadata
    { title :: Text
    , key :: Text
    , rhythm :: Text
    , submitter ::Text
    , transcriber :: Maybe Text
    , abc :: Text
    } deriving (Eq, Show, Generic)


instance ToJSON AbcMetadata
