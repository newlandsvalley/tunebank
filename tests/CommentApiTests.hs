{-# LANGUAGE OverloadedStrings, TypeFamilies, DataKinds,
  DeriveGeneric, TypeOperators #-}

module CommentApiTests (commentApiSpec) where

import Prelude ()
import Prelude.Compat

import qualified Control.Concurrent as C
import Control.Exception (bracket)
import Data.Text (Text, unpack)
import Network.HTTP.Client hiding (Proxy)
import qualified Network.Wai.Handler.Warp as Warp

import Data.Either (isLeft)
import Data.Bifunctor (second)

import Servant
import Servant.Client

import Test.Hspec
import Test.Hspec.Wai

import Tunebank.ApiType (CommentAPI)
import Tunebank.Types (AppCtx(..))
import Tunebank.Server (commentApp)
import Tunebank.Model.User
import Tunebank.Model.Comment
import qualified Tunebank.Model.TuneRef as TuneRef
import Data.Configurator.Types (Config)
import Data.Genre
import TestData

singleComment ::  Genre -> TuneRef.TuneId -> CommentId -> ClientM Comment
commentList ::  Genre -> TuneRef.TuneId -> ClientM CommentList
singleComment :<|> commentList = client (Proxy :: Proxy CommentAPI)

withUserApp :: Config -> IO () -> IO ()
withUserApp config action =
  -- we can spin up a server in another thread and kill that thread when done
  -- in an exception-safe way
  bracket (liftIO $ C.forkIO $ Warp.run 8888 (commentApp $ AppCtx config))
    C.killThread
    (const action)

commentApiSpec :: Config -> Spec
commentApiSpec config =
  -- `around` will start our Server before the tests and turn it off after
  around_ (withUserApp config) $ do
    base <- runIO $ parseBaseUrl "http://localhost:8888"
    mgr <- runIO $ newManager defaultManagerSettings
    let clientEnv = mkClientEnv mgr base

    describe "GET comment" $ do
      it "should get a single comment " $ do
        eResult <- runClientM (singleComment Scandi (TuneRef.tuneId "fastan" "polska") sampleCommentId) clientEnv
        case eResult of
          Left _ ->
            expectationFailure "unexpected single comment error"
          Right c -> do
            (subject c) `shouldBe` "as played by Bert"

    describe "GET comments" $ do
      it "should get a comment list " $ do
        eResult <- runClientM (commentList Scandi (TuneRef.tuneId "fastan" "polska")) clientEnv
        case eResult of
          Left _ ->
            expectationFailure "unexpected comment list error"
          Right cList -> do
            (length $ comment cList) `shouldBe` 3