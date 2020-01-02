module Tunebank.Utils.HTTPErrors where

import Servant.Server
import qualified Data.ByteString.Lazy.UTF8 as UTF8 (fromString)


badRequest :: String -> ServerError
badRequest cause =
  err400 {errBody = UTF8.fromString cause }

notFound :: String -> ServerError
notFound cause =
  err404 {errBody = UTF8.fromString cause }

notAuthorized :: String -> ServerError
notAuthorized cause =
  err401 {errBody = UTF8.fromString cause }
