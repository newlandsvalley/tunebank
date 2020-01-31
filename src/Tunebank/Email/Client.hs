{-# LANGUAGE OverloadedStrings #-}

module Tunebank.Email.Client where

import Prelude ()
import Prelude.Compat
import Control.Monad.Reader


import Data.Text (Text, pack, unpack)
import Data.Text.Lazy (fromStrict)
import Network.HaskellNet.SSL
import Network.HaskellNet.IMAP.SSL
import Network.HaskellNet.SMTP.SSL as SMTP
import Network.HaskellNet.Auth (AuthType(LOGIN))
import Tunebank.Types
import Tunebank.Config
import Tunebank.Model.User (UserId(..))

import Debug.Trace (trace, traceM)

sendConfirmation :: Text -> UserId -> AppM ()
sendConfirmation recipient uid = do
  hostName <- mailHost
  portNumber <- mailPort
  userName <- mailLogin
  password <- mailPassword
  fromAddress <- mailFromAddress
  baseUrl <- thisServerBaseUrl
  let
    settings  = defaultSettingsWithPort portNumber
    subject   = "The traditional tunes database: user validation"
    slug      = show $ userId uid
    link      = baseUrl <> "/tunebank/user/validate/" <> slug
    plainBody = ""
    htmlBody  = fromStrict $ pack $  "Thanks for signing up to the traditional tunes database! <br/><br/>"
               <> "Your account has been created. <br/><br/>"
               <> "Please click this link to activate your account: "
               <> "<a href='" <> link <> "'>"  <> link <> "</a>"

  liftIO $ doSMTPSTARTTLSWithSettings hostName settings $ \c -> do
    _ <- traceM "trying to authenticate"
    authSucceed <- SMTP.authenticate PLAIN userName password c
    if authSucceed
      then do
        _ <- traceM "authentication succeeded - sending the mail"
        sendMimeMail (unpack recipient) userName subject plainBody htmlBody [] c
      else do
        _ <- traceM "authentication failed"
        print "Authentication error."


{- version if we want to xdebug
sendConfirmation :: Text -> UserId -> AppM ()
sendConfirmation recipient uid = do
  hostName <- mailHost
  portNumber <- mailPort
  userName <- mailLogin
  password <- mailPassword
  fromAddress <- mailFromAddress
  baseUrl <- thisServerBaseUrl
  let
    settings  = defaultSettingsWithPort portNumber
    subject   = "The traditional tunes database: user validation"
    slug      = unpack $ userId uid
    link      = baseUrl <> "/tunebank/user/validate/" <> slug
    plainBody = ""
    htmlBody  = fromStrict $ pack $  "Thanks for signing up to the traditional tunes database! <br/><br/>"
               <> "Your account has been created. <br/><br/>"
               <> "Please click this link to activate your account: "
               <> "<a href='" <> link <> "'>"  <> link <> "</a>"
  _ <- traceM "trying to connect"
  c <- liftIO $ connectSMTPSTARTTLSWithSettings hostName settings
  _ <- traceM "trying to authenticate"
  -- authSucceed <- liftIO $ SMTP.authenticate LOGIN userName password c
  authSucceed <- liftIO $ SMTP.authenticate PLAIN userName password c
  if authSucceed
    then do
      _ <- traceM "authentication succeeded - sending the mail"
      liftIO $ sendMimeMail (unpack recipient) userName subject plainBody htmlBody [] c
    else do
      _ <- traceM "authentication failed"
      liftIO $ print "Authentication error."
-}
