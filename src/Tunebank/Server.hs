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
import Data.Text (Text)
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
import Tunebank.TestData.AbcTune (getTuneMetadata, getTuneList, postNewTune)
import Tunebank.TestData.Comment (getTuneComment, getTuneComments)
import Tunebank.ApiType (UserAPI, AbcTuneAPI1, CommentAPI1)
import Tunebank.Model.User (User(..))
import qualified Tunebank.Model.UserRegistration as UserReg (Submission)
import qualified Tunebank.Model.NewTune as NewTune (Submission)
import Tunebank.Model.AbcMetadata (AbcMetadata(..))
import Tunebank.Model.TuneRef (TuneId, TuneRef)
import Tunebank.Model.Comment (CommentId, Comment)
import Tunebank.Authentication.BasicAuth (UserName, basicAuthServerContext)

import Data.Genre (Genre)

userServer :: Server UserAPI
userServer = usersHandler :<|> newUserHandler :<|> checkUserHandler
   where
     usersHandler :: Handler [User]
     usersHandler =
       return getUsers

     newUserHandler :: UserReg.Submission -> Handler User
     newUserHandler submission =
       return $ registerNewUser submission

     -- check user is pre-checked with basic authentication
     -- at the moment, the client doesn't really expect much from the response
     -- once it passes authentication
     checkUserHandler :: UserName -> Handler Text
     checkUserHandler userName =
         return "Y"

tuneServer :: Server AbcTuneAPI1
tuneServer = tuneHandler :<|> tuneListHandler :<|> newTuneHandler
   where
     tuneHandler :: Genre -> TuneId -> Handler AbcMetadata
     tuneHandler  genre tuneId =
       case getTuneMetadata genre tuneId of
         Nothing -> throwError (err404 {errBody = "tune not found"})
         Just metadata -> return metadata

     tuneListHandler :: Genre -> Handler [TuneRef]
     tuneListHandler genre =
       return $ getTuneList genre

     newTuneHandler :: Genre -> NewTune.Submission -> Handler TuneId
     newTuneHandler genre submission =
       case postNewTune genre submission of
         Left err ->
             throwError (err400 {errBody = err})
         Right tuneId -> return tuneId

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
-- not yet a webserver.  userApp is the only one so far to
-- be fitted with basic authentication
userApp :: Application
userApp = serveWithContext
            userAPI basicAuthServerContext userServer

tuneApp :: Application
tuneApp = serve abcTuneAPI tuneServer

commentApp :: Application
commentApp = serve commentAPI commentServer
