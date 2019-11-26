module Tunebank.TestData.Comment
  (
    getTuneComments
  , getTuneComment
  ) where

import Prelude ()
import Prelude.Compat

import Tunebank.Model.Comment (Comment(..), CommentId(..))
import qualified Tunebank.Model.TuneRef as TuneRef
import Data.Time.Calendar
import GHC.Generics
import Data.Text (Text, pack)
import Data.Tuple (snd)
import Data.Genre
import Data.Map (Map, fromList, elems)
import qualified Data.Map as Map (lookup)


import Debug.Trace (trace)

type CommentEntry = (CommentId, Comment)

getTuneComments :: Genre -> TuneRef.TuneId -> [Comment]
getTuneComments genre tuneId =
  map snd commentsList

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
    c1 = CommentId $ pack "c1"
    c2 = CommentId $ pack "c2"
    c3 = CommentId $ pack "c3"
  in
    [ ( c1, Comment c1 (pack "administrator") (pack "as played by Fred") (pack "Fred link") )
    , ( c2, Comment c2 (pack "administrator") (pack "as played by Bert") (pack "Bert link") )
    , ( c3, Comment c3 (pack "john") (pack "as played by Joe") (pack "Joe link") )
    ]
