{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Tunebank.ApiType where

import Data.Text
import Data.Time (UTCTime)
import Servant.API
import Tunebank.Model.User (User)
import Tunebank.Model.AbcMetadata (AbcMetadata)
import Tunebank.Model.TuneRef (TuneId, TuneRef)
import Tunebank.Model.Comment (Comment, CommentId)
import Data.Genre (Genre)
import Tunebank.Model.Genre ()


type UserAPI = "tunebank" :> "users" :>  Get '[JSON] [User]
               -- equivalent to 'GET /tunebank/users'
               :<|> "tunebank" :> "users" :> ReqBody '[JSON] User :> Post '[JSON] User
                -- equivalent to 'POST /tunebank/users' with a JSON object
               :<|> "tunebank" :> "user" :> Capture "userid" String :> DeleteNoContent '[JSON] NoContent
               -- equivalent to 'DELETE /tunebank/user/:username'

type UserAPI1 = "tunebank" :> "users" :> Get '[JSON] [User]

type AbcTuneAPI1 =
     "tunebank" :> "genre" :> Capture "genre" Genre :> "tune" :> Capture "tune" TuneId :> Get '[JSON] AbcMetadata
     :<|> "tunebank" :> "genre" :> Capture "genre" Genre :> "search" :>  Get '[JSON] [TuneRef]

type CommentAPI1 =
    "tunebank" :> "genre" :> Capture "genre" Genre :> "tune" :> Capture "tune" TuneId :> "comment" :> Capture "comment" CommentId :> Get '[JSON] Comment
     :<|>   "tunebank" :> "genre" :> Capture "genre" Genre :> "tune" :> Capture "tune" TuneId :> "comments" :> Get '[JSON] [Comment]
