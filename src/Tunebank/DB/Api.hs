
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
import Data.String
import Servant.Server (ServerError, errBody, err404)
import Data.Pool
import Database.PostgreSQL.Simple
import Data.Text (Text, pack, unpack)
import Tunebank.Types
import Tunebank.DB.Class
import Data.Genre (Genre)
import Tunebank.Model.User (User, UserId, UserName, UserList(..))
import Tunebank.Model.TuneRef (TuneId, TuneList(..), TuneRef, tuneId)
import Tunebank.Model.AbcMetadata
import qualified Tunebank.Model.AbcMetadata as AbcMetadata (Origin(..))
import Tunebank.Model.Comment (CommentId, CommentList(..), Comment)
import qualified Tunebank.Model.UserRegistration as UserReg (Submission)
import qualified Tunebank.Model.TuneText as NewTune (Submission)
import qualified Tunebank.Model.CommentSubmission as NewComment (Submission(..))
import Tunebank.Model.Pagination
import Data.Abc.Validator (normaliseKeySignature, normaliseRhythm)

import Debug.Trace (traceM)


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
      let
        queryTemplate =
          "SELECT id, name, email, password, role, registration_date, valid FROM users WHERE id = ? "
        param =
          Only (uid :: UserId)
      pool <- asks _getPool
      us <- liftIO $ withResource pool
         (\conn -> query conn queryTemplate param)
      pure $ safeHead us

    findUserByName :: Text -> PostgresT IO  (Maybe User)
    findUserByName name = do
      let
        queryTemplate =
          "SELECT id, name, email, password, role, registration_date, valid FROM users WHERE name = ? "
        param =
          Only (name :: Text)
      pool <- asks _getPool
      us <- liftIO $ withResource pool
         (\conn -> query conn queryTemplate param)
      pure $ safeHead us

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
    getUsers limit offset = do
      let
        queryTemplate =
          "SELECT id, name, email, password, role, registration_date, valid FROM users"
           <> " LIMIT ? OFFSET ? "
        params =
          (limit :: Int, offset :: Int)
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

    getTunes ::  Genre -> Int -> Int -> PostgresT IO [TuneRef]
    getTunes genre page size =
      pure []

    countTunes  :: Genre
              -> Maybe Title
              -> Maybe Rhythm
              -> Maybe TuneKey
              -> Maybe Source
              -> Maybe Origin
              -> Maybe Composer
              -> Maybe Transcriber
              -> PostgresT IO Int
    countTunes genre mTitle mRhythm mKey mSource mOrigin mComposer mTranscriber = do
      let
        genreStr = pack $ show genre
        queryBase = "SELECT count(*) from tunes WHERE genre = ? "
        params =
          (Only (genreStr :: Text))
        queryTemplate = queryBase
                      <> optionalParams genre mTitle mRhythm mKey mSource mOrigin mComposer mTranscriber
      -- _ <- traceM ("count tunes query: " <> (show queryTemplate))
      pool <- asks _getPool
      [Only i] <- liftIO $ withResource pool
         (\conn -> query conn queryTemplate params)
      pure i

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
           -> PostgresT IO [TuneRef]
    search genre mTitle mRhythm mKey mSource mOrigin mComposer mTranscriber sortKey limit offset = do
      let
        genreStr = pack $ show genre
        orderBy = case sortKey of
          Alpha -> " ORDER BY title ASC "
          Date -> " ORDER BY creation_ts DESC "
        queryBase = "SELECT tune_id, title, rhythm, abc, "
               <> " to_char(creation_ts, 'DD Mon yyyy')  from tunes WHERE genre = ? "
        pagination = " LIMIT ? OFFSET ? "
        params =
          (genreStr :: Text, limit :: Int, offset :: Int)
        queryTemplate = queryBase
                    <> optionalParams genre mTitle mRhythm mKey mSource mOrigin mComposer mTranscriber
                    <> orderBy
                    <> pagination
      _ <- traceM ("search query: " <> (show queryTemplate))
      pool <- asks _getPool
      liftIO $ withResource pool
         (\conn -> query conn queryTemplate params)

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


safeHead :: [a] -> Maybe a
safeHead as =
  if null as
    then Nothing
    else Just $ head as

-- | optional parameters in a search where clause
-- | key signature and rhythm query parameters are normalised as far as
-- | possible to attempt to match what's held on the database 
optionalParams :: Genre
               -> Maybe Title
               -> Maybe Rhythm
               -> Maybe TuneKey
               -> Maybe Source
               -> Maybe Origin
               -> Maybe Composer
               -> Maybe Transcriber
               -> Query
optionalParams genre mTitle mRhythm mKey mSource mOrigin mComposer mTranscriber =
  let
    generateLikeClause :: String -> Text -> Query
    generateLikeClause name value =
      "and " <> fromString name <> " ILIKE '%" <> fromString (unpack value) <> "%' "

    generateEqClause :: String -> Text -> Query
    generateEqClause name value =
      "and " <> fromString name <> " = '" <> fromString (unpack value) <> "' "

    queryTitle = maybe "" (\(Title x) -> generateLikeClause "title" x) mTitle
    queryRhythm = maybe "" (\(Rhythm x) -> generateEqClause "rhythm" (normaliseRhythm genre x)) mRhythm
    queryTuneKey = maybe "" (\(TuneKey x) -> generateEqClause "keysignature" (normaliseKeySignature x)) mKey
    querySource = maybe "" (\(Source x) -> generateLikeClause "source" x) mSource
    queryOrigin = maybe "" (\(Origin x) -> generateLikeClause "origin" x)  mOrigin
    queryComposer = maybe "" (\(Composer x) -> generateLikeClause "composer" x) mComposer
    queryTranscriber = maybe "" (\(Transcriber x) ->  generateLikeClause "transcriber" x) mTranscriber
  in
    queryTitle <> queryRhythm <> queryTuneKey <> querySource
               <> queryOrigin <> queryComposer <> queryTranscriber
