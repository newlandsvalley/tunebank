{-# LANGUAGE OverloadedStrings #-}

module Tunebank.Email.Client where

import Prelude ()
import Prelude.Compat
import Control.Monad.Reader

import Data.Text (pack, unpack)
import Data.Text.Lazy (fromStrict)
import Network.HaskellNet.SSL
import Network.HaskellNet.IMAP.SSL
import Network.HaskellNet.SMTP.SSL as SMTP
import Tunebank.Types
import Tunebank.Config
import Tunebank.Model.NewUser (EmailConfirmation(..))

import Debug.Trace (traceM)

sendConfirmation :: EmailConfirmation -> AppM ()
sendConfirmation confirmation = do
  hostName <- mailHost
  portNumber <- mailPort
  userName <- mailLogin
  password <- mailPassword
  baseUrl <- thisServerBaseUrl
  let
    settings  = defaultSettingsWithPort portNumber
    subject   = "The traditional tunes database: user validation"
    theSlug   = unpack $ slug confirmation
    link      = baseUrl <> "/tunebank/user/validate/" <> theSlug
    plainBody = ""
    htmlBody  = fromStrict $ pack $  "Thanks for signing up to the traditional tunes database! <br/><br/>"
               <> "Your account has been created. <br/><br/>"
               <> "Please click this link to activate your account: "
               <> "<a href='" <> link <> "'>"  <> link <> "</a>"

  liftIO $ doSMTPSTARTTLSWithSettings hostName settings $ \c -> do
    _ <- traceM ("trying to authenticate with email server - " <> userName <> ":" <> password)
    authSucceed <- SMTP.authenticate PLAIN userName password c
    if authSucceed
      then do
        _ <- traceM "authentication succeeded - sending the mail"
        sendMimeMail (unpack $ address confirmation) userName subject plainBody htmlBody [] c
      else do
        _ <- traceM "authentication failed"
        -- print "Authentication error."
        pure ()
