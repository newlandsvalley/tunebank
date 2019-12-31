module Mock.MockUser
  (
    getUsers
  , countUsers
  , findUserById
  , findUserByName
  , userList
  , validateUser
  ) where

import Prelude ()
import Prelude.Compat hiding (lookup)

import Tunebank.Model.User (User(..), Role(..), UserName(..), UserId(..), UserList(..))
import qualified Tunebank.Model.UserRegistration as Reg (Submission(..))
import Tunebank.Model.Pagination (Pagination(..))
import Data.Text (Text, pack, unpack, toUpper)
import Data.Time.Calendar
import Data.Tuple (snd)
import Data.Map (Map, lookup, fromList)
import Data.Maybe (maybe)
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

getUsers :: Int -> Int -> Int -> UserList
getUsers page size totalUsers =
  let
    users = userList
    maxPages = (totalUsers + size - 1) `quot` size
    pagination = Pagination page size maxPages
  in
    UserList users pagination

findUserById :: UserId -> Maybe User
findUserById userId =
  lookup userId $ fromList usersById

findUserByName :: Text -> Maybe User
findUserByName name =
  lookup name $ fromList usersByName

countUsers :: Int
countUsers =
  length userList

validateUser :: Text -> Text -> Bool
validateUser name suppliedPassword =
  let
    foo = trace ("validating user: " <> (show name)) name
    userMap :: UserMap
    userMap = fromList usersByName
    mpwd = fmap password $ lookup name userMap
  in
    maybe False (== suppliedPassword) mpwd

userList :: [User]
userList =
  [ User (pack "Isaac Newton") (pack "isaac@newton.co.uk") (pack "hide me") NormalUser (fromGregorian 1683  3 1) True (UserId $ pack "NEWTON")
  , User (pack "Albert Einstein") (pack "ae@mc2.org") (pack "hide me") Administrator  (fromGregorian 1905 12 1) True (UserId $ pack "EINSTEIN")
  , User (pack "administrator") (pack "john.watson@gmx.co.uk") (pack "password") Administrator  (fromGregorian 1905 12 1) True (UserId $ pack "ADMINISTRATOR")
  , User (pack "Fred") (pack "fred@bloggs.co.uk") (pack "password") NormalUser (fromGregorian 1683  3 1) True (UserId $ pack "FRED")
  ]
