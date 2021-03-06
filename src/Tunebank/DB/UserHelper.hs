{-# LANGUAGE OverloadedStrings #-}

module Tunebank.DB.UserHelper where

import Data.Text
import Servant.Server (ServerError)
import Tunebank.DB.Class
import Tunebank.Model.User
import qualified Tunebank.Model.NewUser as NewUser (NewUser(..), EmailConfirmation)
import qualified Tunebank.Model.UserRegistration as Reg (Submission(..))
import Tunebank.Utils.HTTPErrors

-- | These 'helper' queries can be run through the database and they
-- | expand the results thus obtained so as to matck the required
-- | signature

-- has the user got admin permission
hasAdminRole :: DBAccess m d => UserName -> m Bool
hasAdminRole (UserName userName) = do
  mUser <- findUserByName userName
  pure $ maybe False (\u -> role u == Administrator) mUser

-- has user the permission to delete a tune or a comment
hasDeletePermission ::  DBAccess m d => UserName -> Text -> m Bool
hasDeletePermission userName submitter =
  if (userName == (UserName submitter))
    then
      pure True
    else
      hasAdminRole userName

-- return the rule of the user (if found)
getUserRole :: DBAccess m d => UserName -> m (Maybe Role)
getUserRole (UserName userName) = do
  mUser <- findUserByName userName
  pure $ fmap role mUser

registerNewUser :: DBAccess m d =>  Reg.Submission -> m (Either ServerError NewUser.EmailConfirmation)
registerNewUser submission =
  let
    uname =
      Reg.name submission
    newUser =
      NewUser.NewUser uname (Reg.email submission) (Reg.password submission)
  in
    do
      mUser <- findUserByName uname
      case mUser of
        Just _ ->
          pure $ Left $ badRequest "user already exists"
        _ -> do
          mConfirmation <- insertUser newUser
          case mConfirmation of
            Just confirmation ->
              pure $ Right confirmation
            _ ->
              pure $ Left $ serverError "user insert failed unexpectedly"


getUsersIfPermitted :: DBAccess m d => UserName -> Int -> Int -> m (Either ServerError [User])
getUsersIfPermitted userName limit offset = do
  canQuery <- hasAdminRole userName
  if canQuery
    then do
      userList <- getUsers limit offset
      pure $ Right userList
    else
      pure $ Left $ notAuthorized ("querying users not allowed for user: " <> (show userName))
