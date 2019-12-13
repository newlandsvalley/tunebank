{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Tunebank.Model.Pagination
  ( Pagination(..)
  , paginationHeaderContent
  ) where


import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text, pack)
import qualified Data.Aeson.Parser

data Pagination = Pagination
  { page :: Int
  , size :: Int
  }
    deriving (Eq, Show, Generic)

instance ToJSON Pagination
instance FromJSON Pagination

paginationHeaderContent :: Pagination -> Text
paginationHeaderContent pagination =
  pack $ "[" <> show (page pagination) <> " of " <> show (size pagination) <> "]"
