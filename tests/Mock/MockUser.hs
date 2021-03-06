
{-# LANGUAGE OverloadedStrings #-}

module Mock.MockUser
  (
    getUsers
  , countUsers
  , findUserById
  , findUserByName
  , userList
  , insertUser
  , validateUser
  ) where

import Prelude ()
import Prelude.Compat hiding (lookup)

import Tunebank.Model.User (User(..), Role(..), UserName(..), UserId(..), UserList(..))
import Tunebank.Model.NewUser (NewUser, EmailConfirmation(..))
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

{-}
getUsers :: Int -> Int -> Int -> UserList
getUsers page size totalUsers =
  let
    users = userList
    maxPages = (totalUsers + size - 1) `quot` size
    pagination = Pagination page size maxPages
  in
    UserList users pagination
-}

getUsers :: Int -> Int -> [User]
getUsers limit offset =
  userList

findUserById :: UserId -> Maybe User
findUserById userId =
  lookup userId $ fromList usersById

findUserByName :: Text -> Maybe User
findUserByName name =
  lookup name $ fromList usersByName

countUsers :: Int
countUsers =
  length userList

insertUser :: NewUser -> Maybe EmailConfirmation
insertUser newUser =
  Just $ EmailConfirmation "john.watson@gmx.uk" "44"

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
  [ User (UserId 1) (pack "Isaac Newton") (pack "isaac@newton.co.uk") (pack "hide me") NormalUser (fromGregorian 1683  3 1) True
  , User (UserId 2) (pack "Albert Einstein") (pack "ae@mc2.org") (pack "hide me") Administrator  (fromGregorian 1905 12 1) True
  , User (UserId 3) (pack "administrator") (pack "john.watson@gmx.co.uk") (pack "password") Administrator  (fromGregorian 1905 12 1) True
  , User (UserId 4) (pack "Fred") (pack "fred@bloggs.co.uk") (pack "password") NormalUser (fromGregorian 1683  3 1) True
  ]
