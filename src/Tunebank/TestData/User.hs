module Tunebank.TestData.User
  (
    getUsers
  , registerNewUser
  , validateUser
  , getUserRole
  , hasAdminRole
  ) where

import Prelude ()
import Prelude.Compat hiding (lookup)

import Tunebank.Model.User (User(..), Role(..), UserName(..))
import qualified Tunebank.Model.UserRegistration as Reg (Submission(..))
import Data.Text (Text, pack, unpack)
import Data.Time.Calendar
import Data.Tuple (snd)
import Data.Map (Map, lookup, fromList)
import Data.Maybe (maybe)
import GHC.Generics


import Debug.Trace (trace)

type UserEntry = (Text, User)
type UserMap = Map Text User

userEntries :: [UserEntry]
userEntries =
  [ ( (pack "Isaac Newton"),  User (pack "Isaac Newton") (pack "isaac@newton.co.uk") (pack "hide me") NormalUser (fromGregorian 1683  3 1) True )
  , ( (pack "Albert Einstein"), User (pack "Albert Einstein") (pack "ae@mc2.org") (pack "hide me") Administrator  (fromGregorian 1905 12 1) True )
  , ( (pack "Administrator"), User (pack "Administrator") (pack "john.watson@gmx.co.uk") (pack "password") Administrator  (fromGregorian 1905 12 1) True )
  ]

getUsers :: [User]
getUsers =
  map snd userEntries

validateUser :: Text -> Text -> Bool
validateUser name suppliedPassword =
  let
    userMap :: UserMap
    userMap = fromList userEntries
    mpwd = fmap password $ lookup name userMap
  in
    maybe False (== suppliedPassword) mpwd

hasAdminRole :: UserName -> Bool
hasAdminRole userName =
  maybe False (== Administrator) (getUserRole userName)

getUserRole :: UserName -> Maybe Role
getUserRole (UserName userName) =
  let
    userMap :: UserMap
    userMap = fromList userEntries
  in
    fmap role $ lookup userName userMap


registerNewUser :: Reg.Submission -> User
registerNewUser submission =
  let
    name = Reg.name submission
    email = Reg.email submission
    password = Reg.password submission
    foo = trace ("Registering new user: " <> (unpack name)) name
  in
    User name email password NormalUser (fromGregorian 1683  3 1) False
