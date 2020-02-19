{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Tunebank.Authentication.BasicAuth
   ( basicAuthServerContext
   ) where


import Servant.API.BasicAuth (BasicAuthData (BasicAuthData))
import Servant.Server (BasicAuthCheck (BasicAuthCheck),
                        BasicAuthResult( Authorized, Unauthorized ),
                        Context ((:.), EmptyContext))
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8)
import Tunebank.Model.User (UserName(..))
import Tunebank.DB.Api (DBConfig(..))
import Data.Pool
import Database.PostgreSQL.Simple (query, Only(..))

import Debug.Trace (traceM)


-- | We need to supply our handlers with the right Context. In this case,
-- Basic Authentication requires a Context Entry with the 'BasicAuthCheck' value
-- tagged with "foo-tag" This context is then supplied to 'server' and threaded
-- to the BasicAuth HasServer handlers.
basicAuthServerContext :: DBConfig -> Context (BasicAuthCheck UserName ': '[])
basicAuthServerContext dbConfig = (authCheck dbConfig) :. EmptyContext

authCheck :: DBConfig -> BasicAuthCheck UserName
authCheck dbConfig =
  let
    basicAuthCheck :: BasicAuthData -> IO (BasicAuthResult UserName)
    basicAuthCheck (BasicAuthData username password) = do
      result <- validateUser dbConfig (decodeUtf8 username) (decodeUtf8 password)
      if result
        then pure $ Authorized (UserName (decodeUtf8 username))
        else pure Unauthorized
  in
    BasicAuthCheck basicAuthCheck

validateUser :: DBConfig -> Text -> Text -> IO Bool
validateUser dbConfig name password =
  let
    pool =
      _getPool dbConfig
    queryTemplate =
      "SELECT count(*) FROM users WHERE name = ? and password = ? and valid = TRUE "
    params =
      (name :: Text, password :: Text)
  in do
    [Only (count :: Integer)]  <- withResource pool
       (\conn -> query conn queryTemplate params)
    _ <- traceM ("Validating user: " <> (unpack name) <> " result: " <> (show count))
    pure $ count == 1
