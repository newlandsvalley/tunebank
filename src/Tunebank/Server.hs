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

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import qualified Data.ByteString.Lazy.UTF8 as UTF8 (fromString)
import Data.ByteString.Internal (packChars)
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
import Network.HTTP.Types.Method (Method)
import Servant
import System.Directory
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8
import Servant.Types.SourceT (source)
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html
import Data.Configurator.Types (Config)
import Data.Configurator


import Control.Monad.Catch (MonadThrow, catch, throwM)
import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT)
import Data.Functor.Identity
import Servant.Server (ServerError)


import Tunebank.Types
import Tunebank.Class
import Tunebank.TestData.User (validateUserTemporary, hasAdminRole)
import Tunebank.TestData.AbcTune (getTuneList, search, postNewTune, getTuneBinary, deleteTune)
import Tunebank.ApiType (UserAPI, AbcTuneAPI, CommentAPI, OverallAPI)
import Tunebank.Model.User (User(..), UserName(..), UserId(..), UserList(..))
import Tunebank.DBHelper.User (registerNewUser)
import Tunebank.DBHelper.Comment (deleteCommentIfPermitted, upsertCommentIfPermitted)
import Tunebank.Utils.HTTPErrors
import qualified Tunebank.Model.UserRegistration as UserReg (Submission)
import qualified Tunebank.Model.TuneText as TuneText (Submission)
import qualified Tunebank.Config as Config
import Tunebank.Model.AbcMetadata hiding (Origin(..))
import qualified Tunebank.Model.AbcMetadata as AbcMetadata (Origin(..))
import Tunebank.Model.TuneRef (TuneId, TuneRef)
import qualified Tunebank.Model.TuneRef as TuneRef (TuneList(..))
import Tunebank.Model.Comment (CommentId, Comment, CommentList(..))
import qualified Tunebank.Model.CommentSubmission as NewComment (Submission(..))
import Tunebank.Model.Pagination
import Tunebank.Authentication.BasicAuth (basicAuthServerContext)
import qualified Tunebank.Email.Client as Email (sendConfirmation)


import Data.Genre (Genre)
import Debug.Trace (trace, traceM)

-- | these two types will eventually be replaced by real database types
data SqlBackend = SqlBackend
type SqlPersistT = ReaderT SqlBackend

instance DBAccess (SqlPersistT IO) SqlBackend where

  runQuery :: SqlBackend -> SqlPersistT IO a -> AppM a
  runQuery conn query =
     -- liftIO $ query
    throwError (err404 {errBody = "database layer not yet implemented"})

  findUserById uid =
    pure Nothing

  findUserByName name =
    pure Nothing

  countUsers =
    pure 0

  getUsers :: Int -> Int -> SqlPersistT IO UserList
  getUsers page size =
    pure $ UserList [] (Pagination 0 0 0)

  insertUser :: User -> SqlPersistT IO Bool
  insertUser user =
    pure False

  updateUser :: UserId -> User -> SqlPersistT IO ()
  updateUser uid user =
    pure ()

  findTuneById :: Genre -> TuneId -> SqlPersistT IO (Maybe AbcMetadata)
  findTuneById genre tuneId =
    pure Nothing

  findCommentById :: Genre -> TuneId -> CommentId -> SqlPersistT IO (Maybe Comment)
  findCommentById genre tuneId commentId =
    pure Nothing

  getComments :: Genre -> TuneId -> SqlPersistT IO CommentList
  getComments genre tuneId =
    pure $ CommentList []

  insertComment :: UserName -> Genre -> TuneId -> NewComment.Submission -> SqlPersistT IO CommentId
  insertComment userName genre tuneId submission =
    pure $ NewComment.cid submission

  deleteComment :: Genre -> TuneId -> CommentId -> SqlPersistT IO ()
  deleteComment genre tuneId commentId =
    pure ()


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
       let
         page = fromMaybe 1 mPage
       size <- Config.getPageSize mSize
       if (not $ hasAdminRole userName)
         then throwError (err404 {errBody = "not authorized"})
         else do
           userList <- runQuery conn $ getUsers page size
           pure $ userList


     newUserHandler :: UserReg.Submission -> AppM Text
     newUserHandler submission = do
       _ <- traceM ("new user: " <> (show submission))
       eUser <- runQuery conn $ registerNewUser submission
       case eUser of
         Left serverError ->
           throwError serverError
           -- (err404 {errBody = ("registration failed: " <> err)})
         Right user -> do
           _ <- Email.sendConfirmation (email user) (uid user)
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
           throwError (err404 {errBody = "user registration not recognized"})
         Just user -> do
           let
             updatedUser = user { valid = True }
           _ <- runQuery conn $ updateUser userId updatedUser
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
      pure "Welcome to tunebank versionn 0.1.0.0."

    tuneHandler :: Genre -> TuneId -> AppM AbcMetadata
    tuneHandler genre tuneId = do
      mMetadata <- runQuery conn $ findTuneById genre tuneId
      case mMetadata of
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
      mMetadata <- runQuery conn $ findTuneById genre tuneId
      case mMetadata of
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
      _ <- traceM ("new tune: " <> (show submission))
      case (postNewTune userName genre submission) of
        Left err ->
          throwError (err400 {errBody = err})
        Right tuneId ->
          pure tuneId

    deleteTuneHandler :: UserName -> Genre -> TuneId -> AppM ()
    deleteTuneHandler userName genre tuneId = do
      _ <- traceM ("delete tune: " <> (show tuneId))
      case (deleteTune userName genre tuneId) of
        Left err ->
          throwError (err400 {errBody = err})
        Right tuneId ->
          pure ()

-- | generic handler for all binary tune formats
binaryHandler :: Transcodable -> Genre -> TuneId -> AppM Lazy.ByteString
binaryHandler binaryFormat genre tuneId = do
  tuneBinary <- getTuneBinary binaryFormat genre tuneId
  case tuneBinary of
    Left err ->
      throwError (err400 {errBody = err})
    Right bytes ->
      pure bytes

commentServer :: DBAccess m d => d -> ServerT CommentAPI AppM
commentServer conn =
  commentHandler :<|> commentListHandler
                :<|> newCommentHandler :<|> deleteCommentHandler
  where
    commentHandler :: Genre -> TuneId -> CommentId -> AppM Comment
    commentHandler  genre tuneId commentId = do
      _ <- traceM ("get comment: " <> (show commentId))
      mComment <- runQuery conn $ findCommentById genre tuneId commentId
      case mComment of
        Nothing ->
          throwError (err404 {errBody = "comment not found"})
        Just comment ->
          pure comment

    commentListHandler :: Genre -> TuneId -> AppM CommentList
    commentListHandler genre tuneId = do
      _ <- traceM ("get comments for: " <> (show tuneId))
      runQuery conn $ getComments genre tuneId

    newCommentHandler :: UserName -> Genre -> TuneId -> NewComment.Submission -> AppM CommentId
    newCommentHandler userName genre tuneId submission = do
      _ <- traceM ("new comment: " <> (show submission))
      eUpserted <- runQuery conn $ upsertCommentIfPermitted userName genre tuneId submission
      case eUpserted of
        Left serverError ->
          throwError serverError
        Right commentId ->
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
-}

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
  corsWithAuthAndDelete $
    serveWithContext overallAPI basicAuthServerContext $
        hoistServerWithContext overallAPI (Proxy :: Proxy (BasicAuthCheck UserName ': '[]))
          (flip runReaderT ctx) (overallServer (SqlBackend))

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
