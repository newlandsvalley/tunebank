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
import Tunebank.Utils.Timestamps
import TestData

import Debug.Trace (trace)

type CommentEntry = (CommentId, Comment)

findCommentById :: Genre -> TuneRef.TuneId -> CommentId -> Maybe Comment
findCommentById genre tuneId commentId =
  case genre of
    Scandi ->
      let
        tracedCommentId = trace ("client looking for scandi commentId: " <> (show commentId)) commentId
      in
        Map.lookup tracedCommentId $ fromList commentsList
    _ ->
      Nothing

getComments :: Genre -> TuneRef.TuneId -> CommentList
getComments genre tuneId =
  if (genre == Scandi && tuneId == TuneRef.tuneId "fastan" "polska") then
    CommentList $ map snd commentsList
  else
    CommentList []

commentsList :: [CommentEntry]
commentsList =
  let
    c1 = sampleExistingCommentId -- 1573030493600 -- submitted by Fred
    c2 = CommentId "1573030493700"
    c3 = CommentId "1573030493800"
    -- tuneId = TuneRef.tuneId "fastan" "polska"
    -- let's attach all comments to the tune with a 'primary key' of 0
    tunePK = 0
  in
    [ ( c1, Comment c1 tunePK "Fred" "as played by Fred" "Fred link" )
    , ( c2, Comment c2 tunePK "administrator" "as played by Bert" "Bert link" )
    , ( c3, Comment c3 tunePK "john" "as played by Joe" "Joe link" )
    ]
