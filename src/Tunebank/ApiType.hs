{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Tunebank.ApiType where

import Data.Text
import Data.Time (UTCTime)
import Servant.API
import Tunebank.Model.User (User)
import qualified Tunebank.Model.UserRegistration as UserReg (Submission)
import qualified Tunebank.Model.NewTune as NewTune (Submission)
import Tunebank.Model.AbcMetadata (AbcMetadata)
import Tunebank.Model.TuneRef (TuneId, TuneRef)
import Tunebank.Model.Comment (Comment, CommentId)
import Tunebank.Model.Genre ()
import Data.Genre (Genre)


type UserAPI = "tunebank" :> "users"
                          :>  Get '[JSON] [User]
               -- equivalent to 'GET /tunebank/users'
               :<|> "tunebank" :> "user"
                               :> ReqBody '[FormUrlEncoded] UserReg.Submission
                               :> Post '[JSON] User
                -- equivalent to 'POST /tunebank/users' with a URL encoded form object

            {- we don't want to expose user deletion
               :<|> "tunebank" :> "user"
                               :> Capture "userid" String
                               :> DeleteNoContent '[JSON] NoContent
               -- equivalent to 'DELETE /tunebank/user/:username'
            -}

type UserAPI1 = "tunebank" :> "users"
                           :> Get '[JSON] [User]

type AbcTuneAPI1 =
     "tunebank" :> "genre"
                :> Capture "genre" Genre
                :> "tune"
                :> Capture "tune" TuneId
                :> Get '[JSON] AbcMetadata

     :<|> "tunebank" :> "genre"
                     :> Capture "genre" Genre
                     :> "search"
                     :>  Get '[JSON] [TuneRef]

     :<|> "tunebank" :> "genre"
                     :> Capture "genre" Genre
                     :> "tune"
                     :> ReqBody '[FormUrlEncoded] NewTune.Submission
                     :> Post '[JSON] TuneId

type CommentAPI1 =
    "tunebank" :> "genre"
               :> Capture "genre" Genre
               :> "tune" :> Capture "tune" TuneId
               :> "comment" :> Capture "comment" CommentId
               :> Get '[JSON] Comment

     :<|>  "tunebank" :> "genre"
                      :> Capture "genre" Genre
                      :> "tune"
                      :> Capture "tune" TuneId
                      :> "comments"
                      :> Get '[JSON] [Comment]

type OverallAPI = UserAPI :<|> AbcTuneAPI1 :<|> CommentAPI1
