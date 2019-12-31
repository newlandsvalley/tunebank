
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Tunebank.Class where

import Control.Monad.Catch (MonadThrow, catch, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Tunebank.Types
import Tunebank.Model.User (UserId, User, UserList)
import Tunebank.Model.AbcMetadata (AbcMetadata)
import Tunebank.Model.TuneRef (TuneId)
import Tunebank.Model.Comment (CommentId, Comment, CommentList)
import Data.Genre (Genre)

-- | This is a very abstract interface into the DB layer allowing
-- | instances in production that access a real database and
-- | instances in test which mock it.

class (MonadThrow m, MonadIO m, Monad m) => DBAccess m d | m -> d, d -> m where

  runQuery :: d -> m a -> AppM a

  findUserById :: UserId -> m (Maybe User)

  findUserByName :: Text -> m (Maybe User)

  countUsers :: m Int

  getUsers ::  Int -> Int -> m UserList

  insertUser :: User -> m Bool

  updateUser :: UserId -> User -> m ()

  findTuneById :: Genre -> TuneId -> m (Maybe AbcMetadata)

  findCommentById :: Genre -> TuneId -> CommentId -> m (Maybe Comment)

  getComments :: Genre -> TuneId -> m CommentList
