module Tunebank.TestData.User
  (
    users1
  ) where

import Prelude ()
import Prelude.Compat

import Tunebank.Model.User (User(..))
import Data.Time.Calendar
import GHC.Generics

users1 :: [User]
users1 =
  [ User "Isaac Newton"   "isaac@newton.co.uk" "normal" (fromGregorian 1683  3 1) True
  , User "Albert Einstein" "ae@mc2.org"  "admin"    (fromGregorian 1905 12 1) True
  ]
