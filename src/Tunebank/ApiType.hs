{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Tunebank.ApiType where

import Data.Text
import Data.Time (UTCTime)
import Data.ByteString.Lazy (ByteString)
import Servant.API
import Tunebank.Model.User (User, UserName, UserId)
import qualified Tunebank.Model.UserRegistration as UserReg (Submission)
import qualified Tunebank.Model.NewTune as NewTune (Submission)
import Tunebank.Types (PDF, PNG, PostScript, MIDI)
import Tunebank.Model.AbcMetadata
import Tunebank.Model.TuneRef (TuneId, TuneRef)
import Tunebank.Model.Comment (Comment, CommentId)
import Tunebank.Model.Genre ()
import Data.Genre (Genre)


type UserAPI = "tunebank" :> "users"
                          :> BasicAuth "tunebank-realm" UserName
                          :>  Get '[JSON] [User]
                -- equivalent to 'GET /tunebank/users'

               :<|> "tunebank" :> "user"
                               :> ReqBody '[FormUrlEncoded] UserReg.Submission
                               :> Post '[JSON] User
                -- equivalent to 'POST /tunebank/users' with a URL encoded from form

               :<|> "tunebank" :> "user" :> "check"
                               :> BasicAuth "tunebank-realm" UserName
                               :> Get '[PlainText] Text
                -- equivalent to GET /tunebank/check/user

               :<|> "tunebank" :> "user" :> "validate"
                               :> Capture "tune" UserId
                               :> Get '[PlainText] Text
               -- equivalent to GET /tunebank/user/validate/slug
               -- validate a user registration

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
                     :> "tune"
                     :> Capture "tune" TuneId
                     :> "pdf"
                     :> Get '[PDF] ByteString

     :<|> "tunebank" :> "genre"
                     :> Capture "genre" Genre
                     :> "tune"
                     :> Capture "tune" TuneId
                     :> "ps"
                     :> Get '[PostScript] ByteString

     :<|> "tunebank" :> "genre"
                     :> Capture "genre" Genre
                     :> "tune"
                     :> Capture "tune" TuneId
                     :> "png"
                     :> Get '[PNG] ByteString

     :<|> "tunebank" :> "genre"
                     :> Capture "genre" Genre
                     :> "tune"
                     :> Capture "tune" TuneId
                     :> "midi"
                     :> Get '[MIDI] ByteString

     :<|> "tunebank" :> "genre"
                     :> Capture "genre" Genre
                     :> "search"
                     :> QueryParam "title" Title
                     :> QueryParam "rhythm" Rhythm
                     :> QueryParam "key" TuneKey
                     :> QueryParam "source" Source
                     :> QueryParam "origin" Origin
                     :> QueryParam "composer" Composer
                     :> QueryParam "transcriber" Transcriber
                     :> QueryParam "sort" SortKey
                     :>  Get '[JSON] [TuneRef]

     :<|> "tunebank" :> BasicAuth "tunebank-realm" UserName
                     :> "genre"
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
