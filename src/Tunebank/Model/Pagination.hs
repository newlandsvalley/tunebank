{-# LANGUAGE DeriveGeneric #-}

module Tunebank.Model.Pagination
  ( Pagination(..)
  ) where    

import GHC.Generics
import Data.Aeson

data Pagination = Pagination
  { page :: Int
  , size :: Int
  , maxPages :: Int
  }
    deriving (Eq, Show, Generic)

instance ToJSON Pagination
instance FromJSON Pagination
