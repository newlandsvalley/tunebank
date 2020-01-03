{-# LANGUAGE OverloadedStrings #-}

module Tunebank.DBHelper.User where

import Data.Text
import Servant.Server (ServerError)
import Control.Monad.IO.Class (liftIO)
import Data.Time.Calendar
import Tunebank.Types
import Tunebank.Class
import Tunebank.Model.User
import qualified Tunebank.Model.UserRegistration as Reg (Submission(..))
import Tunebank.Utils.Timestamps (today)
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

-- we need to provide user checks here
registerNewUser :: DBAccess m d =>  Reg.Submission -> m (Either ServerError User)
registerNewUser submission =
  let
    name = Reg.name submission
    email = Reg.email submission
    password = Reg.password submission
    --  foo = trace ("Registering new user: " <> (unpack name)) name
  in
    do
      mUser <- findUserByName name
      case mUser of
        Just _ ->
          pure $ Left $ badRequest "user already exists"
        _ -> do
          date <- liftIO today
          pure $ Right $ User name email password NormalUser date False (UserId $ toUpper name)

getUsersIfPermitted :: DBAccess m d => UserName -> Int -> Int -> m (Either ServerError UserList)
getUsersIfPermitted userName page size = do
  canQuery <- hasAdminRole userName
  if canQuery
    then do
      userList <- getUsers page size
      pure $ Right userList
    else
      pure $ Left $ notAuthorized ("querying users not allowed for user: " <> (show userName))
