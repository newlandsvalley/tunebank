
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Tunebank.Model.TuneText where

import GHC.Generics
import Data.Text (Text)
import qualified Data.ByteString.Lazy.Char8 as BC
import Web.FormUrlEncoded
import Network.HTTP.Media ((//))
import Servant.API.ContentTypes
import Data.Typeable

-- | a 'raw' ABC tune submitted from a form
data Submission = Submission
  { abc :: Text
  } deriving (Eq, Show, Generic)

instance FromForm Submission

-- required for testing (somehow)
instance ToForm Submission

-- We don't need to use ABC at the moment - we simply rely on PlainText

-- | 'raw' ABC returned from a simple Get request
data ABC deriving Typeable

instance Accept ABC where
  contentType _ = "text" // "vnd.abc"

instance MimeRender ABC String where
  mimeRender _  = BC.pack

instance MimeUnrender ABC String where
  mimeUnrender _ = Right . BC.unpack
