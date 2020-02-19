{-# LANGUAGE OverloadedStrings #-}

module Tunebank.DB.CommentHelper where

import Servant.Server (ServerError)
import Tunebank.DB.Class
import Tunebank.Model.User
import Tunebank.Model.Comment
import qualified Tunebank.Model.CommentSubmission as NewComment (Submission(..))
import qualified Tunebank.Model.TuneRef as TuneRef
import Data.Genre
import Tunebank.Utils.HTTPErrors
import Tunebank.DB.UserHelper

import Debug.Trace (traceM)

deleteCommentIfPermitted :: DBAccess m d => UserName -> Genre -> TuneRef.TuneId -> CommentId  -> m (Either ServerError ())
deleteCommentIfPermitted  userName genre tuneId commentId = do
  mComment <- findCommentById genre tuneId commentId
  mPK <- findTunePrimaryKey genre tuneId
  case mPK of
    Just tunePK ->
      case mComment of
        Nothing ->
          pure $ Left $ notFound ("comment: " <> (show commentId))
        Just comment -> do
          canDelete <- hasDeletePermission userName (submitter comment)
          if canDelete
            then do
              _ <- deleteComment genre tunePK commentId
              pure $ Right ()
            else pure $ Left $ notAuthorized ("deletion not allowed for user: " <> (show userName))
    _ ->
      pure $ Left $ serverError ("could not find tune primary key for " <> (show tuneId))


upsertCommentIfPermitted ::  DBAccess m d => UserName -> Genre -> TuneRef.TuneId -> NewComment.Submission -> m (Either ServerError CommentId)
upsertCommentIfPermitted  userName genre tuneId submission = do
  let
    commentId = NewComment.commentId submission
  _ <- traceM ("upsert comment if permitted ")
  mComment <- findCommentById genre tuneId commentId
  mPK <- findTunePrimaryKey genre tuneId
  case mPK of
    Just tunePK ->
      case mComment of
        Nothing -> do
          let
            comment = buildComment tunePK submission
          _ <- insertComment comment
          pure $ Right commentId
        Just oldComment -> do
          canUpdate <- hasDeletePermission userName (submitter oldComment)
          if canUpdate
            then do
              let
                comment = updateComment oldComment submission
              _ <- deleteComment genre tunePK commentId
              _ <- insertComment comment
              pure $ Right commentId
            else pure $ Left $ notAuthorized ("new comment not allowed for user: " <> (show userName))
    _ ->
      pure $ Left $ serverError ("could not find tune primary key for " <> (show tuneId))

buildComment :: Int -> NewComment.Submission -> Comment
buildComment tunePK submission =
  Comment (NewComment.commentId submission) tunePK  (NewComment.user submission)
             (NewComment.subject submission) (NewComment.text submission)

updateComment :: Comment -> NewComment.Submission -> Comment
updateComment oldComment submission =
  Comment (commentId oldComment) (tidkey oldComment) (submitter oldComment)
           (NewComment.subject submission) (NewComment.text submission)
