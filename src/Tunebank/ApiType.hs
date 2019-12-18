{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Tunebank.ApiType where

import Data.Text
import Data.Time (UTCTime)
import Data.ByteString.Lazy (ByteString)
import Servant.API
import Tunebank.Model.User (User, UserName, UserId, UserList)
import qualified Tunebank.Model.UserRegistration as UserReg (Submission)
import qualified Tunebank.Model.TuneText as TuneText (Submission)
import Tunebank.Types (PDF, PNG, PostScript, MIDI, AcceptMime)
import Tunebank.Model.AbcMetadata
import Tunebank.Model.TuneRef (TuneId, TuneRef, TuneList)
import Tunebank.Model.Comment (Comment, CommentId, CommentList)
import Tunebank.Model.Genre ()
import Data.Genre (Genre)


type UserAPI = "tunebank" :> "user"
                          :> BasicAuth "tunebank-realm" UserName
                          :> QueryParam "page" Int
                          :> QueryParam "size" Int
                          :>  Get '[JSON] UserList
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

type AbcTuneAPI = "tunebank"
                :>  Get '[PlainText] Text

     :<|>  "tunebank" :> "genre"
                :> Capture "genre" Genre
                :> "tune"
                :> Capture "tune" TuneId
                :> Get '[JSON, ABC] AbcMetadata

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
                     :> "tune"
                     :> Capture "tune" TuneId
                     :> "abc"
                     :> Get '[PlainText] Text

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
                     :> QueryParam "page" Int
                     :> QueryParam "size" Int
                     :>  Get '[JSON] TuneList

     :<|> "tunebank" :> BasicAuth "tunebank-realm" UserName
                     :> "genre"
                     :> Capture "genre" Genre
                     :> "tune"
                     :> ReqBody '[FormUrlEncoded] TuneText.Submission
                     :> Post '[JSON] TuneId

type CommentAPI =
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
                      :> Get '[JSON] CommentList

type OverallAPI = UserAPI :<|> AbcTuneAPI :<|> CommentAPI
