{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module Tunebank.Server where

import Prelude ()
import Prelude.Compat hiding (lookup)

import Control.Monad.Reader

import qualified Data.ByteString.Lazy as Lazy (ByteString)
import Data.Maybe
import Data.Text (Text)
import Network.Wai
import Network.Wai.Middleware.Cors
import Servant

import Tunebank.Types
import Tunebank.ApiType (UserAPI, AbcTuneAPI, CommentAPI, OverallAPI)
import Tunebank.Model.User (UserName(..), UserId(..), UserList(..))
import qualified Tunebank.Model.UserRegistration as UserReg (Submission)
import Tunebank.DB.Class
import Tunebank.DB.UserHelper (registerNewUser, getUsersIfPermitted)
import Tunebank.DB.CommentHelper (deleteCommentIfPermitted, upsertCommentIfPermitted, commentToSubmission)
import Tunebank.DB.TuneHelper (deleteTuneIfPermitted, upsertTuneIfPermitted)
import Tunebank.Utils.HTTPErrors (badRequest, badRequestLazy, notFound)
import qualified Tunebank.Model.TuneText as NewTune (Submission)
import qualified Tunebank.Config as Config
import Tunebank.Model.AbcMetadata hiding (Origin(..))
import qualified Tunebank.Model.AbcMetadata as AbcMetadata (Origin(..))
import Tunebank.Model.TuneRef (TuneId(..))
import qualified Tunebank.Model.TuneRef as TuneRef (TuneList(..))
import Tunebank.Model.Comment (CommentId(..))
import qualified Tunebank.Model.CommentSubmission as CommentMsg (Submission(..))
import Tunebank.Model.Pagination
import Tunebank.TypeConversion.Transcode (transcodeTo)
import qualified Tunebank.TypeConversion.Cache as Cache (removeOldFiles)
import Tunebank.Authentication.BasicAuth (basicAuthServerContext)
import qualified Tunebank.Email.Client as Email (sendConfirmation)
import Tunebank.DB.Api


import Data.Genre (Genre)
import Debug.Trace (trace, traceM)

userServer :: DBAccess m d => d -> ServerT UserAPI AppM
userServer conn =
  usersHandler :<|> newUserHandler :<|> checkUserHandler
              :<|> validateUserRegistrationHandler
   where
     usersHandler :: UserName
                  -> Maybe Int
                  -> Maybe Int
                  -> AppM UserList
     usersHandler userName mPage mSize = do
       _ <- traceM ("get users: " <> (show userName))
       limit <- Config.getPageSize mSize
       let
         thisPage = maybe 1 (max 1) mPage
         offset = (thisPage - 1) * limit
       eUsers <- runQuery conn $ getUsersIfPermitted userName limit offset
       userCount <- runQuery conn countUsers
       let
         pageCount = (div userCount limit) + 1
       case eUsers of
         Left serverError -> do
           _ <- traceM ("get users - server error ")
           throwError serverError
         Right usrs -> do
           _ <- traceM ("get users returned rows: " <> (show $ length usrs))
           pure $ UserList usrs (Pagination thisPage limit pageCount)

     newUserHandler :: UserReg.Submission -> AppM Text
     newUserHandler submission = do
       _ <- traceM ("new user: " <> (show submission))
       eUser <- runQuery conn $ registerNewUser submission
       case eUser of
         Left serverError ->
           throwError serverError
           -- (err404 {errBody = ("registration failed: " <> err)})
         Right confirmation -> do
           _ <- Email.sendConfirmation confirmation
           pure "we've sent you an email to complete the registration process"

     -- check user is pre-checked with basic authentication
     -- at the moment, the client doesn't really expect much from the response
     -- once it passes authentication
     checkUserHandler :: UserName -> AppM Text
     checkUserHandler userName = do
       _ <- traceM ("check user: " <> (show userName))
       pure  "Y"

     validateUserRegistrationHandler :: UserId -> AppM Text
     validateUserRegistrationHandler userId = do
        _ <- traceM ("validate user: " <> (show userId))
        mUser <- runQuery conn $ findUserById userId
        case mUser of
          Nothing ->
            throwError $ badRequest "user registration not recognized"
          Just _ -> do
            _ <- runQuery conn $ setUserValidity userId True
            pure "Y"


tuneServer :: DBAccess m d => d -> ServerT AbcTuneAPI AppM
tuneServer conn =
  welcomeHandler  :<|> tuneHandler :<|> tunePdfHandler
    :<|> tunePostScriptHandler  :<|> tunePngHandler :<|> tuneMidiHandler
    :<|> tuneAbcHandler  :<|> tuneListHandler :<|> newTuneHandler
    :<|> deleteTuneHandler
  where
    welcomeHandler :: AppM Text
    welcomeHandler =
      pure "Welcome to tunebank version 0.1.0.0."

    tuneHandler :: Genre -> TuneId -> AppM AbcMetadata
    tuneHandler genre tuneId = do
      mMetadata <- runQuery conn $ findTuneById genre tuneId
      case mMetadata of
        Nothing -> do
          throwError $ notFound ("not found tune: " <> show tuneId)
        Just metadata ->
          pure metadata

    tunePdfHandler :: Genre -> TuneId -> AppM Lazy.ByteString
    tunePdfHandler genre tuneId =
      binaryHandler conn Pdf genre tuneId

    tunePostScriptHandler :: Genre -> TuneId -> AppM Lazy.ByteString
    tunePostScriptHandler genre tuneId =
      binaryHandler conn PostScript genre tuneId

    tunePngHandler :: Genre -> TuneId -> AppM Lazy.ByteString
    tunePngHandler genre tuneId =
      binaryHandler conn Png genre tuneId

    tuneMidiHandler :: Genre -> TuneId -> AppM Lazy.ByteString
    tuneMidiHandler genre tuneId =
      binaryHandler conn Midi genre tuneId

    tuneAbcHandler :: Genre -> TuneId -> AppM Text
    tuneAbcHandler genre tuneId = do
      mMetadata <- runQuery conn $ findTuneById genre tuneId
      case mMetadata of
        Nothing ->
          throwError $ notFound ("not found tune: " <> show tuneId)
        Just metadata ->
          -- pure $ (abcHeaders metadata) <> (abcBody metadata)
          pure (abc metadata)

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
      limit <- Config.getPageSize mSize
      tuneCount <- runQuery conn $ countTunes genre mTitle mRhythm mKey
                        mSource mOrigin  mComposer mTranscriber
      let
        thisPage = maybe 1 (max 1) mPage
        offset = (thisPage - 1) * limit
        sortKey = fromMaybe Alpha mSortKey
        pageCount = (div tuneCount limit) + 1
      tunes <- runQuery conn $
          search genre mTitle mRhythm mKey mSource mOrigin
             mComposer mTranscriber sortKey limit offset
      pure $ TuneRef.TuneList tunes (Pagination thisPage limit pageCount)

    newTuneHandler :: UserName -> Genre -> NewTune.Submission -> AppM Text
    newTuneHandler userName genre submission = do
      _ <- traceM ("new tune: " <> (show submission))
      eTuneId <- runQuery conn $ upsertTuneIfPermitted userName genre submission
      case eTuneId of
        Left serverError ->
          throwError serverError
        Right (TuneId tid) -> do
          Cache.removeOldFiles genre (TuneId tid)
          pure tid

    deleteTuneHandler :: UserName -> Genre -> TuneId -> AppM ()
    deleteTuneHandler userName genre tuneId = do
      _ <- traceM ("delete tune: " <> (show tuneId))
      ePermitted <- runQuery conn $ deleteTuneIfPermitted userName genre tuneId
      case ePermitted of
        Left serverError ->
          throwError serverError
        Right _ -> do
          Cache.removeOldFiles genre tuneId
          pure ()

-- | find the requested tune and transcode to the requested binary format
binaryHandler :: DBAccess m d => d -> Transcodable -> Genre -> TuneId -> AppM Lazy.ByteString
binaryHandler conn binaryFormat genre tuneId = do
  mMetadata <- runQuery conn $ findTuneById genre tuneId
  case mMetadata of
    Nothing ->
      throwError $ notFound ("binary handler not found tune: " <> show tuneId)
    Just metadata -> do
      eTranscoded <- transcodeTo binaryFormat genre metadata
      case eTranscoded of
        Left errorBytes -> do
          _ <- traceM ("binary handler for " <> (show binaryFormat) <> " error: " <> (show errorBytes))
          throwError $ badRequestLazy errorBytes
        Right binary ->
          pure binary


commentServer :: DBAccess m d => d -> ServerT CommentAPI AppM
commentServer conn =
  commentHandler :<|> commentListHandler
                :<|> newCommentHandler :<|> deleteCommentHandler
  where
    commentHandler :: Genre -> TuneId -> CommentId -> AppM CommentMsg.Submission
    commentHandler  genre tuneId commentId = do
      _ <- traceM ("get comment: " <> (show commentId))
      mComment <- runQuery conn $ findCommentById genre tuneId commentId
      case mComment of
        Nothing ->
          throwError $ notFound "comment not found"
        Just comment -> do
          let
            commentMsg = commentToSubmission comment
          _ <- traceM ("returning comment: " <> (show commentMsg))
          pure commentMsg

    commentListHandler :: Genre -> TuneId -> AppM [CommentMsg.Submission]
    commentListHandler genre tuneId = do
      _ <- traceM ("get comments for: " <> (show tuneId))
      runQuery conn $ getComments genre tuneId

    newCommentHandler :: UserName -> Genre -> TuneId -> CommentMsg.Submission -> AppM Text
    newCommentHandler userName genre tuneId submission = do
      _ <- traceM ("new comment: " <> (show submission))
      eUpserted <- runQuery conn $ upsertCommentIfPermitted userName genre tuneId submission
      case eUpserted of
        Left serverError ->
          throwError serverError
        Right (CommentId commentId) ->
          pure commentId

    deleteCommentHandler :: UserName -> Genre -> TuneId -> CommentId -> AppM ()
    deleteCommentHandler userName genre tuneId commentId = do
      _ <- traceM ("delete comment: " <> (show commentId))
      ePermitted <- runQuery conn $ deleteCommentIfPermitted userName genre tuneId commentId
      case ePermitted of
        Left serverError ->
          throwError serverError
        Right _ ->
          pure ()

overallServer ::  DBAccess m d => d -> ServerT OverallAPI AppM
overallServer conn =
  (userServer conn ) :<|> (tuneServer conn) :<|> (commentServer conn)

userAPI :: Proxy UserAPI
userAPI = Proxy

tuneAPI :: Proxy AbcTuneAPI
tuneAPI = Proxy

commentAPI :: Proxy CommentAPI
commentAPI = Proxy

overallAPI :: Proxy OverallAPI
overallAPI = Proxy


-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.  userApp is the only one so far to
-- be fitted with basic authentication

-- these next three apps are only for use in the test module
-- and will eventually be moved there


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

fullApp :: AppCtx -> DBConfig -> Application
fullApp ctx dbConfig =
  -- simpleCors $
  corsWithAuthAndDelete $
    serveWithContext overallAPI (basicAuthServerContext dbConfig) $
        hoistServerWithContext overallAPI (Proxy :: Proxy (BasicAuthCheck UserName ': '[]))
          (flip runReaderT ctx) (overallServer dbConfig)

-- | extend simple CORS with the following (which would otherwise be banned):
-- | * Authorization request header
-- | * DELETE HTTP method
corsWithAuthAndDelete :: Middleware
corsWithAuthAndDelete = cors (const $ Just policy)
  where
    methods = simpleMethods <> ["DELETE"]
    policy = simpleCorsResourcePolicy
     { corsRequestHeaders = ["Authorization"]
     , corsMethods = methods
     }
