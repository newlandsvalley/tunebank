module Tunebank.TestData.User
  (
    validateUserTemporary
  , getUserRole
  , hasAdminRole
  , hasDeletePermission
  ) where

-- | this moduld will; be removed completely once we have a database layer


import Prelude ()
import Prelude.Compat hiding (lookup)

import Tunebank.Model.User (User(..), Role(..), UserName(..), UserId(..), UserList(..))
import qualified Tunebank.Model.UserRegistration as Reg (Submission(..))
import Tunebank.Model.Pagination (Pagination(..))
import Data.Text (Text, pack, unpack, toUpper)
import Data.Time.Calendar
import Data.Tuple (snd)
import Data.Map (Map, lookup, fromList)
import Data.Maybe (maybe, isJust)
import Data.ByteString.Lazy.Internal (ByteString)
import GHC.Generics


import Debug.Trace (trace)

type KeyedByName = (Text, User)
type KeyedById = (UserId, User)
type UserMap = Map Text User
type UserRegMap = Map UserId User

usersByName :: [KeyedByName]
usersByName =
  map (\u -> (name u, u)) userList

usersById :: [KeyedById]
usersById =
  map (\u -> (uid u, u)) userList

validateUserTemporary :: Text -> Text -> Bool
validateUserTemporary name suppliedPassword =
  let
    foo = trace ("validating user: " <> (show name)) name
    userMap :: UserMap
    userMap = fromList usersByName
    mpwd = fmap password $ lookup name userMap
  in
    maybe False (== suppliedPassword) mpwd

hasAdminRole :: UserName -> Bool
hasAdminRole userName =
  maybe False (== Administrator) (getUserRole userName)

-- permission to delete a tune or a comment
hasDeletePermission :: UserName -> Text -> Bool
hasDeletePermission userName submitter =
  hasAdminRole userName || userName == (UserName submitter)

getUserRole :: UserName -> Maybe Role
getUserRole (UserName userName) =
  let
    foo = trace ("get user role for: " <> (show userName)) userName
    userMap :: UserMap
    userMap = fromList usersByName
  in
    fmap role $ lookup userName userMap


userList :: [User]
userList =
  [ User (pack "Isaac Newton") (pack "isaac@newton.co.uk") (pack "hide me") NormalUser (fromGregorian 1683  3 1) True (UserId $ pack "NEWTON")
  , User (pack "Albert Einstein") (pack "ae@mc2.org") (pack "hide me") Administrator  (fromGregorian 1905 12 1) True (UserId $ pack "EINSTEIN")
  , User (pack "administrator") (pack "john.watson@gmx.co.uk") (pack "password") Administrator  (fromGregorian 1905 12 1) True (UserId $ pack "ADMINISTRATOR")
  , User (pack "Fred") (pack "fred@bloggs.co.uk") (pack "password") NormalUser (fromGregorian 1683  3 1) True (UserId $ pack "FRED")
  ]
