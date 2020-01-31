{-# LANGUAGE OverloadedStrings #-}

module Tunebank.DB.CommentHelper where

import Servant.Server (ServerError)
import Tunebank.Types
import Tunebank.DB.Class
import Tunebank.Model.User
import Tunebank.Model.Comment
import qualified Tunebank.Model.CommentSubmission as NewComment (Submission(..))
import qualified Tunebank.Model.TuneRef as TuneRef
import Tunebank.Utils.Timestamps (today)
import Data.Genre
import Tunebank.Utils.HTTPErrors
import Tunebank.DB.UserHelper


deleteCommentIfPermitted :: DBAccess m d => UserName -> Genre -> TuneRef.TuneId -> CommentId  -> m (Either ServerError ())
deleteCommentIfPermitted  userName genre tuneId commentId = do
  mComment <- findCommentById genre tuneId commentId
  case mComment of
    Nothing ->
      pure $ Left $ notFound ("comment: " <> (show commentId))
    Just comment -> do
      canDelete <- hasDeletePermission userName (user comment)
      if canDelete
        then do
          _ <- deleteComment genre tuneId commentId
          pure $ Right ()
        else pure $ Left $ notAuthorized ("deletion not allowed for user: " <> (show userName))

upsertCommentIfPermitted ::  DBAccess m d => UserName -> Genre -> TuneRef.TuneId -> NewComment.Submission -> m (Either ServerError CommentId)
upsertCommentIfPermitted  userName genre tuneId submission = do
  let
    commentId = NewComment.cid submission
  mComment <- findCommentById genre tuneId commentId
  case mComment of
    Nothing -> do
      _ <- insertComment userName genre tuneId submission
      pure $ Right commentId
    Just comment -> do
      canUpdate <- hasDeletePermission userName (user comment)
      if canUpdate
        then do
          _ <- deleteComment genre tuneId commentId
          _ <- insertComment userName genre tuneId submission
          pure $ Right commentId
        else pure $ Left $ notAuthorized ("new comment not allowed for user: " <> (show userName))
