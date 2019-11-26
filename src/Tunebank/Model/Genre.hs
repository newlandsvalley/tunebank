module Tunebank.Model.Genre where

import Data.Genre
import Data.GenreParser (genreParse)
import Web.Internal.HttpApiData
import Data.Text (pack, unpack)
import Data.Bifunctor (first)

-- this instance supports Capture text of type Genre

instance FromHttpApiData Genre
  where
    parseUrlPiece text = first pack $ genreParse (unpack text)
