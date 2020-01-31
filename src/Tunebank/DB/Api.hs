
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Tunebank.DB.Api where


import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Catch (MonadThrow, catch, throwM)
import Control.Monad.Fail (MonadFail)
import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class (liftIO)
import Servant.Server (ServerError, errBody, err404)
import Data.Pool
import Database.PostgreSQL.Simple
import Data.Text (Text)
import Tunebank.Types
import Tunebank.DB.Class
import Data.Genre (Genre)
import Tunebank.Model.User (User, UserId, UserName, UserList(..))
import Tunebank.Model.TuneRef (TuneId, TuneList(..), tuneId)
import Tunebank.Model.AbcMetadata
import qualified Tunebank.Model.AbcMetadata as AbcMetadata (Origin(..))
import Tunebank.Model.Comment (CommentId, CommentList(..), Comment)
import qualified Tunebank.Model.UserRegistration as UserReg (Submission)
import qualified Tunebank.Model.TuneText as NewTune (Submission)
import qualified Tunebank.Model.CommentSubmission as NewComment (Submission(..))
import Tunebank.Model.Pagination


-- | This is just a simple newtype wrapper for our 'IORef'.
newtype DBConfig = DBConfig {
  _getPool :: Pool Connection
  }

-- | This is also a simple newtype wrapper for our DB Monad.  This is very
-- similar to Persistent's 'SqlPersistT' type.
newtype PostgresT m a = PostgresT (ReaderT DBConfig m a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader DBConfig, MonadThrow, MonadFail)

instance DBAccess (PostgresT IO) DBConfig where

    runQuery :: DBConfig -> PostgresT IO a -> AppM a
    runQuery dbConfig (PostgresT readerT) = do
       liftIO (runReaderT readerT dbConfig)
           `catch` \(err::ServerError) ->  throwError (err404 {errBody = "got an error trying to read DBConfig"})

    findUserById :: UserId -> PostgresT IO (Maybe User)
    findUserById uid = do
      pool <- asks _getPool
      pure Nothing

    findUserByName :: Text -> PostgresT IO  (Maybe User)
    findUserByName name =
      pure Nothing

    countUsers :: PostgresT IO Int
    countUsers = do
     let
       queryTemplate =
         "SELECT COUNT (*) FROM users "
     pool <- asks _getPool
     [Only i] <- liftIO $ withResource pool
        (\conn -> query_ conn queryTemplate)
     pure i

    getUsers :: Int -> Int -> PostgresT IO [User]
    getUsers page size = do
      let
        offset = size * page
        queryTemplate =
          "SELECT id, name, email, password, role, registration_date, valid FROM users"
           <> " LIMIT ? OFFSET ? "
        params =
          (size :: Int, offset :: Int)
      pool <- asks _getPool
      liftIO $ withResource pool
         (\conn -> query conn queryTemplate params)


      -- pure $ UserList [] (Pagination 0 0 0)

    insertUser :: User -> PostgresT IO Bool
    insertUser user =
      pure False

    updateUser :: UserId -> User -> PostgresT IO ()
    updateUser uid user =
      pure ()

    findTuneById :: Genre -> TuneId -> PostgresT IO (Maybe AbcMetadata)
    findTuneById genre tuneId =
      pure Nothing

    getTunes ::  Genre -> Int -> Int -> PostgresT IO TuneList
    getTunes genre page size =
      pure $ TuneList [] (Pagination 0 0 0)

    search :: Genre
           -> Maybe Title
           -> Maybe Rhythm
           -> Maybe TuneKey
           -> Maybe Source
           -> Maybe AbcMetadata.Origin
           -> Maybe Composer
           -> Maybe Transcriber
           -> SortKey
           -> Int
           -> Int
           -> PostgresT IO TuneList
    search genre mTitle mRhythm mKey mSource mOrigin mComposer mTranscriber sort page size =
      pure $ TuneList [] (Pagination 0 0 0)

    insertTune :: UserName -> Genre -> NewTune.Submission -> PostgresT IO TuneId
    insertTune userName genre submission =
      pure $ tuneId "not" "implemented"

    deleteTune :: Genre -> TuneId -> PostgresT IO ()
    deleteTune genre tuneId =
      pure ()

    findCommentById :: Genre -> TuneId -> CommentId -> PostgresT IO (Maybe Comment)
    findCommentById genre tuneId commentId =
      pure Nothing

    getComments :: Genre -> TuneId -> PostgresT IO CommentList
    getComments genre tuneId =
      pure $ CommentList []

    insertComment :: UserName -> Genre -> TuneId -> NewComment.Submission -> PostgresT IO CommentId
    insertComment userName genre tuneId submission =
      pure $ NewComment.cid submission

    deleteComment :: Genre -> TuneId -> CommentId -> PostgresT IO ()
    deleteComment genre tuneId commentId =
      pure ()