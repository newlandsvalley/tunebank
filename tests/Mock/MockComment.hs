
{-# LANGUAGE OverloadedStrings #-}

module Mock.MockComment where

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
import Data.ByteString.Lazy.Internal (ByteString, packChars)
import qualified Data.Map as Map (lookup)
import qualified Tunebank.Model.TuneRef as TuneRef
import qualified Tunebank.Model.CommentSubmission as NewComment (Submission(..))
import Tunebank.Model.User (UserName(..))
import Tunebank.TestData.User (hasDeletePermission)
import Tunebank.Utils.Timestamps

import Debug.Trace (trace)

type CommentEntry = (CommentId, Comment)

findCommentById :: Genre -> TuneRef.TuneId -> CommentId -> Maybe Comment
findCommentById genre tuneId commentId =
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
