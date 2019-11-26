{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Tunebank.Server where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8
import Servant.Types.SourceT (source)
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html

import Tunebank.TestData.User (users1)
import Tunebank.TestData.AbcMetadata (getTuneMetadata, getTuneList)
import Tunebank.ApiType (UserAPI1, AbcTuneAPI1)
import Tunebank.Model.User (User(..))
import Tunebank.Model.AbcMetadata (AbcMetadata(..))
import Tunebank.Model.TuneRef (TuneId, TuneRef)

import Data.Genre (Genre)

server1 :: Server UserAPI1
server1 = return users1

server2 :: Server AbcTuneAPI1
server2 = tuneHandler :<|> tuneListHandler
   where
     tuneHandler :: Genre -> TuneId -> Handler AbcMetadata
     tuneHandler  genre tuneRef =
       case getTuneMetadata genre tuneRef of
         Nothing -> throwError (err404 {errBody = "tune not found"})
         Just metadata -> return metadata

     tuneListHandler :: Genre -> Handler [TuneRef]
     tuneListHandler genre =
       return $ getTuneList genre

userAPI :: Proxy UserAPI1
userAPI = Proxy

abcTuneAPI :: Proxy AbcTuneAPI1
abcTuneAPI = Proxy

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app1 :: Application
app1 = serve userAPI server1


app2 :: Application
app2 = serve abcTuneAPI server2
