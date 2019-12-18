{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Tunebank.Server where

import Prelude ()
import Prelude.Compat hiding (lookup)

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import Data.List hiding (lookup)
import Data.Maybe
import Data.Text (Text, pack)
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Servant
import System.Directory
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8
import Servant.Types.SourceT (source)
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html
import Data.Configurator.Types (Config)
import Data.Configurator

import Tunebank.TestData.User (getUsers, registerNewUser, validateUserRegistration, hasAdminRole)
import Tunebank.TestData.AbcTune (getTuneMetadata, getTuneList, search, postNewTune, getTuneBinary)
import Tunebank.TestData.Comment (getTuneComment, getTuneComments)
import Tunebank.ApiType (UserAPI, AbcTuneAPI, CommentAPI, OverallAPI)
import Tunebank.Model.User (User(..), UserName(..), UserId(..), UserList(..))
import qualified Tunebank.Model.UserRegistration as UserReg (Submission)
import qualified Tunebank.Model.TuneText as TuneText (Submission)
import Tunebank.Types
import qualified Tunebank.Config as Config
import Tunebank.Model.AbcMetadata hiding (Origin(..))
import qualified Tunebank.Model.AbcMetadata as AbcMetadata (Origin(..))
import Tunebank.Model.TuneRef (TuneId, TuneRef)
import qualified Tunebank.Model.TuneRef as TuneRef (TuneList(..))
import Tunebank.Model.Comment (CommentId, Comment)
-- import Tunebank.Model.Pagination (paginationHeaderContent)
import Tunebank.Authentication.BasicAuth (basicAuthServerContext)


import Data.Genre (Genre)
import Debug.Trace (trace, traceM)


userServer :: ServerT UserAPI AppM
userServer = usersHandler :<|> newUserHandler :<|> checkUserHandler
              :<|> validateUserRegistrationHandler
   where
     usersHandler :: UserName
                  -> Maybe Int
                  -> Maybe Int
                  -> AppM UserList
     usersHandler userName mPage mSize = do
       let
         page = fromMaybe 1 mPage
       size <- Config.getPageSize mSize
       if (not $ hasAdminRole userName)
         then throwError (err404 {errBody = "not authorized"})
         else do
           let
             userList = getUsers page size
           pure $ userList

     newUserHandler :: UserReg.Submission -> AppM User
     newUserHandler submission = do
       pure $ registerNewUser submission

     -- check user is pre-checked with basic authentication
     -- at the moment, the client doesn't really expect much from the response
     -- once it passes authentication
     checkUserHandler :: UserName -> AppM Text
     checkUserHandler userName = do
       _ <- traceM ("check user: " <> (show userName))
       pure  "Y"

     validateUserRegistrationHandler :: UserId -> AppM Text
     validateUserRegistrationHandler userId = do
       if (not $ validateUserRegistration userId)
         then throwError (err404 {errBody = "user registration not recognized"})
         else
           pure "Y"

tuneServer :: ServerT AbcTuneAPI AppM
tuneServer =  welcomeHandler
              :<|> tuneHandler :<|> tunePdfHandler :<|> tunePostScriptHandler
              :<|> tunePngHandler :<|> tuneMidiHandler
              :<|> tuneAbcHandler
              :<|> tuneListHandler :<|> newTuneHandler
  where
    welcomeHandler :: AppM Text
    welcomeHandler =
      pure "Welcome to tunebank versionn 0.1.0.0."

    tuneHandler :: Genre -> TuneId -> AppM AbcMetadata
    tuneHandler genre tuneId = do
      -- let's just prove that lookup config works OK
      sorcePath <- Config.transcodeSourcePath genre
      case (getTuneMetadata genre tuneId) of
        Nothing -> do
          throwError (err404 {errBody = "tune not found"})
        Just metadata ->
          pure metadata

    tunePdfHandler :: Genre -> TuneId -> AppM Lazy.ByteString
    tunePdfHandler genre tuneId =
      binaryHandler Pdf genre tuneId

    tunePostScriptHandler :: Genre -> TuneId -> AppM Lazy.ByteString
    tunePostScriptHandler genre tuneId =
      binaryHandler PostScript genre tuneId

    tunePngHandler :: Genre -> TuneId -> AppM Lazy.ByteString
    tunePngHandler genre tuneId =
      binaryHandler Png genre tuneId

    tuneMidiHandler :: Genre -> TuneId -> AppM Lazy.ByteString
    tuneMidiHandler genre tuneId =
      binaryHandler Midi genre tuneId

    tuneAbcHandler :: Genre -> TuneId -> AppM Text
    tuneAbcHandler genre tuneId = do
      case (getTuneMetadata genre tuneId) of
        Nothing -> do
          throwError (err404 {errBody = "tune not found"})
        Just metadata -> do
          let
            abcText = (abcHeaders metadata) <> (abcBody metadata)
          pure abcText


    tuneListHandler :: Genre
                    -> Maybe Title
                    -> Maybe Rhythm
                    -> Maybe TuneKey
                    -> Maybe Source
                    -> Maybe AbcMetadata.Origin
                    -> Maybe Composer
                    -> Maybe Transcriber
                    -> Maybe SortKey
                    -> Maybe Int
                    -> Maybe Int
                    -> AppM  TuneRef.TuneList
    tuneListHandler genre mTitle mRhythm mKey mSource mOrigin
                     mComposer mTranscriber mSortKey mPage mSize = do
      size <- Config.getPageSize mSize
      let
        page = fromMaybe 1 mPage
        tuneList =
          search genre mTitle mRhythm mKey mSource mOrigin
               mComposer mTranscriber mSortKey page size
      pure $ tuneList

    newTuneHandler :: UserName -> Genre -> TuneText.Submission -> AppM TuneId
    newTuneHandler userName genre submission = do
      case (postNewTune userName genre submission) of
        Left err ->
          throwError (err400 {errBody = err})
        Right tuneId ->
          pure tuneId

-- | generic handler for all binary tune formats
binaryHandler :: Transcodable -> Genre -> TuneId -> AppM Lazy.ByteString
binaryHandler binaryFormat genre tuneId = do
  tuneBinary <- getTuneBinary binaryFormat genre tuneId
  case tuneBinary of
    Left err ->
      throwError (err400 {errBody = err})
    Right bytes ->
      pure bytes

commentServer :: ServerT CommentAPI AppM
commentServer = commentHandler :<|> commentListHandler
  where
    commentHandler :: Genre -> TuneId -> CommentId -> AppM Comment
    commentHandler  genre tuneId commentId = do
      case (getTuneComment genre tuneId commentId) of
        Nothing ->
          throwError (err404 {errBody = "comment not found"})
        Just comment ->
          pure comment

    commentListHandler :: Genre -> TuneId -> AppM [Comment]
    commentListHandler genre tuneId =
      pure $ getTuneComments genre tuneId

overallServer :: ServerT OverallAPI AppM
overallServer =
  userServer :<|> tuneServer :<|> commentServer

userAPI :: Proxy UserAPI
userAPI = Proxy

abcTuneAPI :: Proxy AbcTuneAPI
abcTuneAPI = Proxy

commentAPI :: Proxy CommentAPI
commentAPI = Proxy

overallAPI :: Proxy OverallAPI
overallAPI = Proxy


-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.  userApp is the only one so far to
-- be fitted with basic authentication
userApp :: AppCtx -> Application
userApp ctx =
  serveWithContext userAPI basicAuthServerContext $
    hoistServerWithContext userAPI (Proxy :: Proxy (BasicAuthCheck UserName ': '[]))
      (flip runReaderT ctx) userServer

tuneApp :: AppCtx -> Application
tuneApp ctx =
  serveWithContext abcTuneAPI basicAuthServerContext $
    hoistServerWithContext abcTuneAPI (Proxy :: Proxy (BasicAuthCheck UserName ': '[]))
      (flip runReaderT ctx) tuneServer

commentApp :: AppCtx -> Application
commentApp ctx =
  serveWithContext commentAPI basicAuthServerContext $
    hoistServerWithContext commentAPI (Proxy :: Proxy (BasicAuthCheck UserName ': '[]))
      (flip runReaderT ctx) commentServer

{-}
fullApp :: AppCtx -> Application
fullApp ctx =
  cors (const $ Just policy)
    $ provideOptions overallAPI
    $ serveWithContext overallAPI basicAuthServerContext $
        hoistServerWithContext overallAPI (Proxy :: Proxy (BasicAuthCheck UserName ': '[]))
          (flip runReaderT ctx) overallServer
  where
    policy = simpleCorsResourcePolicy
           { corsRequestHeaders = [ "content-type" ] }
-}

fullApp :: AppCtx -> Application
fullApp ctx =
  -- simpleCors $
  corsWithAuth $
    serveWithContext overallAPI basicAuthServerContext $
        hoistServerWithContext overallAPI (Proxy :: Proxy (BasicAuthCheck UserName ': '[]))
          (flip runReaderT ctx) overallServer

-- | Allow Auth header with CORS.  This would be otherwise banned.
corsWithAuth :: Middleware
corsWithAuth = cors (const $ Just policy)
  where
    policy = simpleCorsResourcePolicy
     { corsRequestHeaders = ["Authorization"] }
