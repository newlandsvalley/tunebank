
{-# LANGUAGE OverloadedStrings #-}

module Tunebank.TestData.Comment
  (
    getTuneComments
  , getTuneComment
  ) where

-- | the only test data comments we have belong tp fastan, polska

import Prelude ()
import Prelude.Compat

import Tunebank.Model.Comment (Comment(..), CommentId(..), CommentList(..))
import qualified Tunebank.Model.TuneRef as TuneRef
import Data.Time.Calendar
import GHC.Generics
import Data.Text (Text, pack)
import Data.Tuple (snd)
import Data.Genre
import Data.Map (Map, fromList, elems)
import qualified Data.Map as Map (lookup)
import qualified Tunebank.Model.TuneRef as TuneRef
import Tunebank.Utils.Timestamps


import Debug.Trace (trace)

type CommentEntry = (CommentId, Comment)

getTuneComments :: Genre -> TuneRef.TuneId -> CommentList
getTuneComments genre tuneId =
  if (genre == Scandi && tuneId == TuneRef.tuneId "fastan" "polska") then
    CommentList $ map snd commentsList
  else
    CommentList []

getTuneComment :: Genre -> TuneRef.TuneId -> CommentId -> Maybe Comment
getTuneComment genre tuneId commentId =
  case genre of
    Scandi ->
      let
        tracedCommentId = trace ("scandi commentId: " <> (show commentId)) commentId
      in
        Map.lookup tracedCommentId $ fromList commentsList
    _ ->
      Nothing

commentsList :: [CommentEntry]
commentsList =
  let
    c1 = CommentId $ day2timestamp $ (fromGregorian 2019  12 10)
    c2 = CommentId $ day2timestamp $ (fromGregorian 2019  12 14)
    c3 = CommentId $ day2timestamp $ (fromGregorian 2019  12 17)
    tuneId = TuneRef.tuneId "fastan" "polska"
  in
    [ ( c1, Comment c1 tuneId "administrator" "as played by Fred" "Fred link" )
    , ( c2, Comment c2 tuneId "administrator" "as played by Bert" "Bert link" )
    , ( c3, Comment c3 tuneId "john" "as played by Joe" "Joe link" )
    ]
