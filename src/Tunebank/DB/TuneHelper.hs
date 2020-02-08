{-# LANGUAGE OverloadedStrings #-}

module Tunebank.DB.TuneHelper where


import Control.Monad.IO.Class (liftIO)
import Servant.Server (ServerError)
import Tunebank.Types
import Tunebank.DB.Class
import Tunebank.Model.User
import Tunebank.Model.Comment
import qualified Tunebank.Model.AbcMetadata as AbcMetadata (AbcMetadata(..))
import Tunebank.Model.AbcMetadataSubmission (AbcMetadataSubmission(..), buildMetadata)
import qualified Tunebank.Model.TuneRef as TuneRef
import qualified Tunebank.Model.TuneText as NewTune (Submission(..))
import Tunebank.TypeConversion.Transcode (transcodeTo)
import Tunebank.Utils.Timestamps (today)
import Data.Genre
import Tunebank.Utils.HTTPErrors
import Tunebank.DB.UserHelper
import Tunebank.Utils.Timestamps (timeNow)


deleteTuneIfPermitted :: DBAccess m d => UserName -> Genre -> TuneRef.TuneId -> m (Either ServerError ())
deleteTuneIfPermitted  userName genre tuneId  = do
  mTune <- findTuneById genre tuneId
  case mTune of
    Nothing ->
      pure $ Left $ notFound ("tune: " <> (show tuneId))
    Just tune -> do
      canDelete <- hasDeletePermission userName (AbcMetadata.submitter tune)
      if canDelete
        then do
          _ <- deleteTune genre tuneId
          pure $ Right ()
        else pure $ Left $ notAuthorized ("deletion not allowed for user: " <> (show userName))

upsertTuneIfPermitted ::  DBAccess m d => UserName -> Genre -> NewTune.Submission -> m (Either ServerError TuneRef.TuneId)
upsertTuneIfPermitted userName genre submission = do
  time <- liftIO $ timeNow
  let
    eMetadata = buildMetadata userName genre (NewTune.abc submission)
  case eMetadata of
    Left errorText -> do
      pure $ Left $ badRequest errorText
    Right newMetadata -> do
      let
        tuneId = TuneRef.tuneId (title newMetadata) (rhythm newMetadata)
      mTune <- findTuneById genre tuneId
      case mTune of
        Nothing -> do
          insertTune newMetadata
          pure $ Right tuneId
        Just oldMetadata -> do
          canUpdate <- hasDeletePermission userName (AbcMetadata.submitter oldMetadata)
          if canUpdate
            then do
              _ <- deleteTune genre tuneId
              _<-  insertTune newMetadata
              pure $ Right tuneId
            else pure $ Left $ notAuthorized ("tune update not allowed for user: " <> (show userName))

{-}
getTuneBinary :: Transcodable -> Genre -> TuneRef.TuneId -> m (Either ServerError ByteString)
getTuneBinary binaryFormat genre tuneId = do
  mTuneMeta <- findTuneById genre tuneId
  case mTuneMeta of
    Nothing ->
      pure $ Left $ badRequest  "tune not found"
    Just metadata ->
      transcodeTo binaryFormat genre metadata
-}
