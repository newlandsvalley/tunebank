
{-# LANGUAGE DeriveGeneric #-}

module Tunebank.Model.TuneRef where

import Data.Time.Calendar
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Aeson.Parser
import Web.Internal.HttpApiData
import Data.Text (Text, pack, toLower)
import Data.Either (Either(..))

-- | the unique ID of a tune (within a genre)
newtype TuneId = TuneId Text
  deriving (Eq, Ord, Show, Generic)

instance ToJSON TuneId

-- | this instance supports Capture text of type TuneId which are
-- | all in one piece when found in URL components
instance FromHttpApiData TuneId
  where
    parseUrlPiece t = Right $ TuneId t

-- | but a TuneId is built by combining the title and rhythm
tuneId :: Text -> Text -> TuneId
tuneId title rhythm =
  TuneId ((toLower title) <> (pack "-") <> (toLower rhythm))

-- | this is data returned within tune lists
data TuneRef = TuneRef
   { uri   :: TuneId
   , title :: Text
   , rhythm :: Text
   , abc :: Text
   , ts  :: Day
   } deriving (Eq, Show, Generic)

instance ToJSON TuneRef
