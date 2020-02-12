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

module Mock.MockBasicAuth
   ( basicAuthServerContext
   ) where

import Servant.API.BasicAuth  (BasicAuthData (BasicAuthData))
import Servant.Server (BasicAuthCheck (BasicAuthCheck),
                       BasicAuthResult( Authorized, Unauthorized ),
                       Context ((:.), EmptyContext))
import Servant.Server.Experimental.Auth()
import Mock.MockUser (validateUser)
import Data.Text.Encoding (decodeUtf8)
import Tunebank.Model.User (UserName(..))


-- | We need to supply our handlers with the right Context. In this case,
-- Basic Authentication requires a Context Entry with the 'BasicAuthCheck' value
-- tagged with "foo-tag" This context is then supplied to 'server' and threaded
-- to the BasicAuth HasServer handlers.
basicAuthServerContext :: Context (BasicAuthCheck UserName ': '[])
basicAuthServerContext = authCheck :. EmptyContext

-- | 'BasicAuthCheck' holds the handler we'll use to verify a username and password.
authCheck :: BasicAuthCheck UserName
authCheck =
  let check (BasicAuthData username password) =
        if (validateUser (decodeUtf8 username) (decodeUtf8 password))
        then return (Authorized (UserName (decodeUtf8 username)))
        else return Unauthorized
  in BasicAuthCheck check
