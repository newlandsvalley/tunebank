{-# LANGUAGE OverloadedStrings, TypeFamilies, DataKinds,
  DeriveGeneric, TypeOperators #-}

module CommentApiTests (commentApiSpec) where

import Prelude ()
import Prelude.Compat

import qualified Control.Concurrent as C
import Control.Exception (bracket)
import Control.Monad.Reader
import Data.IORef (IORef, newIORef)
import Network.HTTP.Client hiding (Proxy)
import qualified Network.Wai.Handler.Warp as Warp

import Data.Either (isLeft)
import Data.Text (Text)

import Servant
import Servant.Client

import Test.Hspec

import Tunebank.ApiType (CommentAPI)
import Tunebank.Types (AppCtx(..))
import Tunebank.Server (commentServer, commentAPI)
import Tunebank.Model.User
import Tunebank.Model.Comment
import qualified Tunebank.Model.CommentSubmission as NewComment (Submission(..))
import qualified Tunebank.Model.TuneRef as TuneRef
import Data.Configurator.Types (Config)
import Data.Genre
import TestData
import Mock.DBState as MockDB
import qualified Mock.MockBasicAuth as MockAuth (basicAuthServerContext)


import Debug.Trace (trace, traceM)

fixtureDelay :: Int
fixtureDelay =
  -- 100 ms
  100000

singleComment :: Genre -> TuneRef.TuneId -> CommentId -> ClientM Comment
commentList ::  Genre -> TuneRef.TuneId -> ClientM CommentList
postComment ::   BasicAuthData -> Genre -> TuneRef.TuneId -> NewComment.Submission -> ClientM Text
deleteComment ::   BasicAuthData -> Genre -> TuneRef.TuneId -> CommentId -> ClientM ()
singleComment :<|> commentList :<|> postComment :<|> deleteComment = client (Proxy :: Proxy CommentAPI)


commentApp :: IORef MockDB.DBState ->  AppCtx -> Application
commentApp dbRef ctx =
  serveWithContext commentAPI MockAuth.basicAuthServerContext $
    hoistServerWithContext commentAPI (Proxy :: Proxy (BasicAuthCheck UserName ': '[]))
      (flip runReaderT ctx) (commentServer $ MockDB.DBIORef dbRef)

withCommentApp :: Config -> IO () -> IO ()
withCommentApp config action = do
  dbRef <- newIORef MockDB.mockedDBState
  -- we can spin up a server in another thread and kill that thread when done
  -- in an exception-safe way
  bracket (do
            _ <- liftIO $ C.threadDelay fixtureDelay
            thread <- liftIO $ C.forkIO $ Warp.run 8888 (commentApp dbRef $ AppCtx config)
            _ <- liftIO $ C.threadDelay fixtureDelay
            pure thread
          )
    C.killThread
    (const action)

commentApiSpec :: Config -> Spec
commentApiSpec config =
  -- `around` will start our Server before the tests and turn it off after
  around_ (withCommentApp config) $ do
    base <- runIO $ parseBaseUrl "http://localhost:8888"
    mgr <- runIO $ newManager defaultManagerSettings
    let clientEnv = mkClientEnv mgr base

    describe "GET comment" $ do
      it "should get a single comment " $ do
        eResult <- runClientM (singleComment Scandi (TuneRef.tuneId "fastan" "polska") sampleExistingCommentId) clientEnv
        case eResult of
          Left err ->
            expectationFailure ("unexpected single comment error: " <> (show err))
          Right c -> do
            (title c) `shouldBe` "as played by Fred"

    describe "GET comments" $ do
      it "should get a comment list " $ do
        eResult <- runClientM (commentList Scandi (TuneRef.tuneId "fastan" "polska")) clientEnv
        case eResult of
          Left _ ->
            expectationFailure "unexpected comment list error"
          Right cList -> do
            (length $ comment cList) `shouldBe` 3

    describe "POST comment" $ do
      it "should post a comment " $ do
        _ <- traceM ("new client comment: " <> (show sampleNewComment))
        eResult <- runClientM (postComment normalUser Scandi (TuneRef.tuneId "fastan" "polska") sampleNewComment) clientEnv
        case eResult of
          Left _ ->
            expectationFailure "unexpected post comment error"
          Right commentId -> do
            commentId `shouldBe` sampleNewCommentSlug

    describe "DELETE comment" $ do
      it "is allowed by an administrator" $ do
        eresult <- runClientM (deleteComment admin Scandi augustssonId sampleExistingCommentId) clientEnv
        -- eresult `shouldBe` (Right ())
        case eresult of
          Right result ->
            result `shouldBe` ()
          Left err -> do
            expectationFailure ("unexpected failure to delete by admin: " <> (show err))
      it "is barred for a normal user who didn't submit the comment" $ do
        eresult <- runClientM (deleteComment normalUser Scandi augustssonId sampleExistingCommentId) clientEnv
        case eresult of
          Left _ ->
            (isLeft eresult) `shouldBe` True
          Right _ -> do
            expectationFailure "unexpected deletion by non-owner"
