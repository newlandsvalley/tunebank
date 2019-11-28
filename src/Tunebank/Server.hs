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

import Tunebank.TestData.User (getUsers, registerNewUser)
import Tunebank.TestData.AbcMetadata (getTuneMetadata, getTuneList)
import Tunebank.TestData.Comment (getTuneComment, getTuneComments)
import Tunebank.ApiType (UserAPI, AbcTuneAPI1, CommentAPI1)
import Tunebank.Model.User (User(..))
import qualified Tunebank.Model.UserRegistration as UserReg (Submission)
import Tunebank.Model.AbcMetadata (AbcMetadata(..))
import Tunebank.Model.TuneRef (TuneId, TuneRef)
import Tunebank.Model.Comment (CommentId, Comment)

import Data.Genre (Genre)

userServer :: Server UserAPI
userServer = usersHandler :<|> newUserHandler
   where
     usersHandler :: Handler [User]
     usersHandler =
       return getUsers

     newUserHandler :: UserReg.Submission -> Handler User
     newUserHandler submission =
       return $ registerNewUser submission

tuneServer :: Server AbcTuneAPI1
tuneServer = tuneHandler :<|> tuneListHandler
   where
     tuneHandler :: Genre -> TuneId -> Handler AbcMetadata
     tuneHandler  genre tuneId =
       case getTuneMetadata genre tuneId of
         Nothing -> throwError (err404 {errBody = "tune not found"})
         Just metadata -> return metadata

     tuneListHandler :: Genre -> Handler [TuneRef]
     tuneListHandler genre =
       return $ getTuneList genre

commentServer :: Server CommentAPI1
commentServer = commentHandler :<|> commentListHandler
  where
    commentHandler :: Genre -> TuneId -> CommentId -> Handler Comment
    commentHandler  genre tuneId commentId =
      case getTuneComment genre tuneId commentId of
        Nothing -> throwError (err404 {errBody = "comment not found"})
        Just comment -> return comment

    commentListHandler :: Genre -> TuneId -> Handler [Comment]
    commentListHandler genre tuneId =
      return $ getTuneComments genre tuneId

userAPI :: Proxy UserAPI
userAPI = Proxy

abcTuneAPI :: Proxy AbcTuneAPI1
abcTuneAPI = Proxy

commentAPI :: Proxy CommentAPI1
commentAPI = Proxy

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
userApp :: Application
userApp = serve userAPI userServer

tuneApp :: Application
tuneApp = serve abcTuneAPI tuneServer

commentApp :: Application
commentApp = serve commentAPI commentServer
