
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Tunebank.DB.Class where

import Control.Monad.Catch (MonadThrow, catch, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Tunebank.Types
import Tunebank.Model.User (UserId, UserName, User, UserList)
import Tunebank.Model.AbcMetadata
import Tunebank.Model.AbcMetadataSubmission
import Tunebank.Model.TuneRef (TuneId, TuneList, TuneRef)
import qualified Tunebank.Model.TuneText as NewTune (Submission)
import Tunebank.Model.Comment (CommentId, Comment, CommentList)
import qualified Tunebank.Model.CommentSubmission as NewComment (Submission)
import Data.Genre (Genre)

-- | This is a very abstract interface into the DB layer allowing
-- | instances in production that access a real database and
-- | instances in test which mock it.

class (MonadThrow m, MonadIO m, Monad m) => DBAccess m d | m -> d, d -> m where

  runQuery :: d -> m a -> AppM a

  findUserById :: UserId -> m (Maybe User)

  findUserByName :: Text -> m (Maybe User)

  countUsers :: m Int

  getUsers ::  Int -> Int -> m [User]

  insertUser :: User -> m Bool

  updateUser :: UserId -> User  -> m ()

  findTuneById :: Genre -> TuneId -> m (Maybe AbcMetadata)

  getTunes ::  Genre -> Int -> Int -> m [TuneRef]

  countTunes  :: Genre
            -> Maybe Title
            -> Maybe Rhythm
            -> Maybe TuneKey
            -> Maybe Source
            -> Maybe Origin
            -> Maybe Composer
            -> Maybe Transcriber
            -> m Int

  search :: Genre
         -> Maybe Title
         -> Maybe Rhythm
         -> Maybe TuneKey
         -> Maybe Source
         -> Maybe Origin
         -> Maybe Composer
         -> Maybe Transcriber
         -> SortKey
         -> Int
         -> Int
         -> m [TuneRef]

  insertTune :: AbcMetadataSubmission -> m TuneId

  deleteTune :: Genre -> TuneId -> m ()

  findCommentById :: Genre -> TuneId -> CommentId -> m (Maybe Comment)

  getComments :: Genre -> TuneId -> m CommentList

  insertComment :: UserName -> Genre -> TuneId -> NewComment.Submission -> m CommentId

  deleteComment :: Genre -> TuneId -> CommentId -> m ()
