{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Mock.DBState where


import Control.Monad (when)
import Control.Monad.Catch (MonadThrow, catch)
import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Servant.Server (ServerError, errBody, err404)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Tunebank.Model.User
import Tunebank.Model.TuneRef (tuneId, TuneList(..))
import Tunebank.Model.CommentSubmission
import Tunebank.Model.Pagination
import Tunebank.DB.Class
import qualified Mock.MockUser as MockUser
import qualified Mock.MockTune as MockTune
import qualified Mock.MockComment as MockComment

data DBState = DBState {
  users :: [User]
}

mockedDBState = DBState (MockUser.userList)

-- | This is just a simple newtype wrapper for our 'IORef'.
newtype DBIORef = DBIORef { unDBIORef :: IORef DBState }

-- | This is also a simple newtype wrapper for our DB Monad.  This is very
-- similar to Persistent's 'SqlPersistT' type.
newtype DB m a = DB (ReaderT DBIORef m a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader DBIORef, MonadThrow)


instance DBAccess (DB IO) DBIORef where

   runQuery dbIORef (DB readerT) =
        liftIO (runReaderT readerT dbIORef)
            `catch` \(err::ServerError) ->  throwError (err404 {errBody = "got an error trying to read DBIORef"})

   findUserById uid =
     pure $ MockUser.findUserById uid

   findUserByName name =
     pure $ MockUser.findUserByName name

   countUsers =
     pure $ MockUser.countUsers

   getUsers page size = do
     pure $ MockUser.getUsers page size

   insertUser newUser =
     -- we're not mocking inserts
     pure $ MockUser.insertUser newUser

   setUserValidity uid validity =
     -- we're not mocking updates
     pure ()

   findTuneById genre tuneId =
     pure $ MockTune.findTuneById genre tuneId

   getTunes genre page size =
     pure $ MockTune.getTuneList genre

   search genre mTitle mRhythm mKey mSource mOrigin mComposer mTranscriber sort page size =
     pure $ MockTune.search  genre mTitle mRhythm mKey mSource mOrigin mComposer mTranscriber sort page size

   countTunes genre mTitle mRhythm mKey mSource mOrigin mComposer mTranscriber =
     pure $ MockTune.countTunes genre mTitle mRhythm mKey mSource mOrigin mComposer mTranscriber

   insertTune submission =
     -- we're not actually doing inserts - just pretending so and returning the tune id
     pure $ MockTune.insertTune submission

   deleteTune genre tuneId =
     -- we're not mocking deletes
     pure 0

   findCommentById  genre tuneId commentId =
      pure $ MockComment.findCommentById genre tuneId commentId

   getComments genre tuneId =
      pure $ MockComment.getComments genre tuneId

   insertComment userName genre tuneId submission =
     pure (cid submission)

   deleteComment genre tuneId commentId =
     -- we're not mocking deletes
     pure ()
