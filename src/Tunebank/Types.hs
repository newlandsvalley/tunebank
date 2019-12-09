
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Tunebank.Types where

import Control.Monad.Reader
import Data.ByteString.Lazy (ByteString)
import Servant.Server (Handler)
import Network.HTTP.Media ((//), (/:))
import Servant.API.ContentTypes
import Data.Configurator.Types (Config)
import Web.Internal.HttpApiData
import Data.Text (Text)
import Data.Genre (Genre)
import Data.Either (Either(..))
import Data.Typeable

type AppM = ReaderT AppCtx Handler

data AppCtx = AppCtx {
  _getConfig :: Config
  }

data PDF deriving Typeable
data PostScript deriving Typeable
data PNG deriving Typeable
data MIDI deriving Typeable

instance Accept PDF where
  contentType _ = "application" // "pdf"

instance MimeRender PDF ByteString where
  mimeRender _  = id

instance MimeUnrender PDF ByteString where
  mimeUnrender _ = Right . id


instance Accept PostScript where
  contentType _ = "application" // "postscript"

instance MimeRender PostScript ByteString where
  mimeRender _  = id

instance MimeUnrender PostScript ByteString where
  mimeUnrender _ = Right . id


instance Accept PNG where
  contentType _ = "image" // "png"

instance MimeRender PNG ByteString where
  mimeRender _  = id

instance MimeUnrender PNG ByteString where
  mimeUnrender _ = Right . id


instance Accept MIDI where
  contentType _ = "audio" // "midi"

instance MimeRender MIDI ByteString where
  mimeRender _  = id

instance MimeUnrender MIDI ByteString where
  mimeUnrender _ = Right . id

-- | A Transcoding Target type
data Transcodable
  = Pdf
  | PostScript
  | Png
  | Midi
    deriving (Eq, Ord, Show, Enum)

-- | an Accept request header contents
newtype AcceptMime =  AcceptMime Text
  deriving (Eq, Ord, Show)

instance FromHttpApiData AcceptMime
  where
    parseUrlPiece  = Right . AcceptMime

-- required for client testing
instance ToHttpApiData AcceptMime
  where
    toUrlPiece (AcceptMime v) = v
