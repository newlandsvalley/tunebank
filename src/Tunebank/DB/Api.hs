
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
import Control.Monad.Catch (MonadThrow, catch)
import Control.Monad.Fail (MonadFail)
import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.String
import Data.Int (Int64)
import Servant.Server (ServerError, errBody, err404)
import Data.Pool
import Database.PostgreSQL.Simple
import Data.Text (Text, pack, unpack)
import Tunebank.Types
import Tunebank.DB.Class
import Data.Genre (Genre)
import Tunebank.Model.User (User, UserId(..), UserName)
import Tunebank.Model.NewUser (NewUser(..), EmailConfirmation(..))
import Tunebank.Model.TuneRef (TuneId(..), TuneRef)
import Tunebank.Model.AbcMetadata
import qualified Tunebank.Model.AbcMetadata as AbcMetadata (Origin(..))
import qualified Tunebank.Model.AbcMetadataSubmission as NewTune (AbcMetadataSubmission(..))
import Tunebank.Model.Comment (CommentId(..), Comment(..))
import qualified Tunebank.Model.CommentSubmission as CommentMsg (Submission(..))
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

    insertUser :: NewUser -> PostgresT IO (Maybe EmailConfirmation)
    insertUser newUser = do
      let
        queryTemplate =
          "INSERT INTO Users (name, email, password) "
          <>  " VALUES (?, ?, ? ) returning email, id::text "
      pool <- asks _getPool
      confirmation :: [EmailConfirmation] <-  liftIO $ withResource pool
         (\conn -> query conn queryTemplate newUser)
      _ <- traceM ("production insert user returned confirmation ")
      pure $ safeHead confirmation

    setUserValidity :: UserId -> Bool -> PostgresT IO ()
    setUserValidity (UserId uid) validity = do
      let
        queryTemplate =
          "UPDATE users set validity = ? where id = ? "
        params =
          (validity :: Bool, uid :: Int)
      pool <- asks _getPool
      rowcount <-  liftIO $ withResource pool
           (\conn -> execute conn queryTemplate params)
      _ <- traceM ("production set user validity for user: " <> (show uid))
      pure ()

    findTuneById :: Genre -> TuneId -> PostgresT IO (Maybe AbcMetadata)
    findTuneById genre (TuneId tid) = do
      let
        genreStr = pack $ show genre
        queryTemplate = "SELECT title, rhythm, keysignature, submitter, "
                <> " floor ( extract ( epoch from creation_ts) * 1000) :: text, "
                <> " abc, source, origin, composer, transcriber "
                <> " from tunes"
                <> " WHERE genre = ? and tune_id = ? "
        params =
          (genreStr :: Text, tid :: Text)
      _ <- traceM ("find tune by id query: " <> (show queryTemplate))
      _ <- traceM ("find tune by id target: " <> (unpack tid))
      pool <- asks _getPool
      ts <- liftIO $ withResource pool
         (\conn -> query conn queryTemplate params)
      pure $ safeHead ts

    findTunePrimaryKey :: Genre -> TuneId -> PostgresT IO (Maybe Int)
    findTunePrimaryKey genre (TuneId tid) = do
      let
        genreStr = pack $ show genre
        queryTemplate = "SELECT id from tunes  "
                     <> " WHERE genre = ? and tune_id = ? "
        params =
          (genreStr :: Text, tid :: Text)
      pool <- asks _getPool
      [Only id]  <- liftIO $ withResource pool
          (\conn -> query conn queryTemplate params)
      pure id

    getTunes ::  Genre -> Int -> Int -> PostgresT IO [TuneRef]
    getTunes genre page size =
      search genre Nothing Nothing Nothing Nothing Nothing Nothing Nothing
             Alpha page size

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
               <> " floor ( extract ( epoch from creation_ts) * 1000) :: text "
               <> " from tunes WHERE genre = ? "
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

    insertTune :: NewTune.AbcMetadataSubmission -> PostgresT IO TuneId
    insertTune metadata = do
      let
        queryTemplate =
          "INSERT INTO Tunes (genre, tune_id, submitter, title, rhythm, keysignature, "
          <> " abc, origin, source, composer, transcriber ) "
          <>  " VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) "
      pool <- asks _getPool
      rowcount <-  liftIO $ withResource pool
         (\conn -> execute conn queryTemplate metadata)
      _ <- traceM ("production insert tune returned id: " <> (show (NewTune.tuneId metadata)))
      pure (NewTune.tuneId metadata)

    deleteTune :: Genre -> TuneId -> PostgresT IO Int64
    deleteTune genre (TuneId tid) = do
      let
        genreStr = pack $ show genre
        queryTemplate = "DELETE FROM tunes "
                    <> " WHERE genre = ? AND tune_id = ? "
        params =
          (genreStr :: Text, tid :: Text)
      pool <- asks _getPool
      liftIO $ withResource pool
         (\conn -> execute conn queryTemplate params)
      --pure ()

    findCommentById :: Genre -> TuneId -> CommentId -> PostgresT IO (Maybe Comment)
    findCommentById genre (TuneId tid)  (CommentId commentId) = do
      let
        queryTemplate =
          "SELECT c.id, c.tid, c.submitter, c.subject, c.text "
          <> " FROM comments c, tunes t "
          <> " WHERE c.tid = t.id "
          <>  " AND t.tune_id = ? and c.id = ? "
        params =
          (tid :: Text, commentId :: Text)
      pool <- asks _getPool
      ts <- liftIO $ withResource pool
            (\conn -> query conn queryTemplate params)
      pure $ safeHead ts

    getComments :: Genre -> TuneId -> PostgresT IO [CommentMsg.Submission]
    getComments genre (TuneId tid) = do
      let
        queryTemplate =
          "SELECT c.submitter, c.id, c.subject, c.text "
          <> " FROM comments c, tunes t "
          <> " WHERE c.tid = t.id "
          <>  " AND t.tune_id = ?  "
        params =
            (Only (tid :: Text))
      pool <- asks _getPool
      liftIO $ withResource pool
        (\conn -> query conn queryTemplate params)

    insertComment :: Comment-> PostgresT IO CommentId
    insertComment comment = do
      let
        queryTemplate =
          "INSERT INTO comments (id, tid, submitter, subject, text) "
          <>  " VALUES (?, ?, ?, ?, ?) "
      pool <- asks _getPool
      rowcount <-  liftIO $ withResource pool
         (\conn -> execute conn queryTemplate comment)
      pure $ (commentId comment)

    deleteComment :: Genre -> Int -> CommentId -> PostgresT IO Int64
    deleteComment genre tunePK (CommentId commentId) = do
      let
        queryTemplate = "DELETE FROM comments "
                    <> " WHERE id = ? AND tid = ? "
        params =
          (commentId :: Text, tunePK :: Int)
      pool <- asks _getPool
      liftIO $ withResource pool
         (\conn -> execute conn queryTemplate params)



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
