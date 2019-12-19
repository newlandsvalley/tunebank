{-# LANGUAGE OverloadedStrings, TypeFamilies, DataKinds,
  DeriveGeneric, TypeOperators #-}

module TuneApiTests (tuneApiSpec) where

import Prelude ()
import Prelude.Compat

import qualified Control.Concurrent as C
import Control.Exception (bracket)
import Data.Text (Text, pack, unpack)
import Network.HTTP.Client hiding (Proxy)
import qualified Network.Wai.Handler.Warp as Warp

import Data.Either (isLeft)
import Data.Bifunctor (first, second)
import Data.Genre (Genre(..))

import Servant
import Servant.Client

import Test.Hspec
import Test.Hspec.Wai

import Data.ByteString.Lazy (ByteString)

import Tunebank.ApiType (AbcTuneAPI)
import Tunebank.Types
import Tunebank.Server (tuneApp)
import Tunebank.Model.User
import Tunebank.Model.TuneText
import Tunebank.Model.TuneRef
import qualified Tunebank.Model.AbcMetadata as Meta
import Data.Configurator.Types (Config)
import TestData

welcome :: ClientM Text
tune ::  Genre -> TuneId -> ClientM Meta.AbcMetadata
tunePdf  ::  Genre -> TuneId -> ClientM ByteString
tunePs   ::  Genre -> TuneId -> ClientM ByteString
tunePng  ::  Genre -> TuneId -> ClientM ByteString
tuneMidi ::  Genre -> TuneId -> ClientM ByteString
tuneAbc  ::  Genre -> TuneId -> ClientM Text
tuneList ::  Genre
       -> Maybe Meta.Title
       -> Maybe Meta.Rhythm
       -> Maybe Meta.TuneKey
       -> Maybe Meta.Source
       -> Maybe Meta.Origin
       -> Maybe Meta.Composer
       -> Maybe Meta.Transcriber
       -> Maybe Meta.SortKey
       -> Maybe Int
       -> Maybe Int
       -> ClientM TuneList
newTune :: BasicAuthData -> Genre -> Submission -> ClientM TuneId
deleteTune :: BasicAuthData -> Genre ->  TuneId -> ClientM ()
welcome :<|> tune :<|> tunePdf :<|> tunePs :<|> tunePng
      :<|> tuneMidi :<|> tuneAbc
      :<|> tuneList :<|> newTune
      :<|> deleteTune = client (Proxy :: Proxy AbcTuneAPI)


withUserApp :: Config -> IO () -> IO ()
withUserApp config action =
  -- we can spin up a server in another thread and kill that thread when done
  -- in an exception-safe way
  bracket (liftIO $ C.forkIO $ Warp.run 8888 (tuneApp $ AppCtx config))
    C.killThread
    (const action)

tuneApiSpec :: Config -> Spec
tuneApiSpec config =
  -- `around` will start our Server before the tests and turn it off after
  around_ (withUserApp config) $ do
    base <- runIO $ parseBaseUrl "http://localhost:8888"
    mgr <- runIO $ newManager defaultManagerSettings
    let clientEnv = mkClientEnv mgr base

    describe "Welcome" $ do
      it "should provide a wecome message" $ do
        result <- runClientM welcome clientEnv
        result `shouldBe` (Right  "Welcome to tunebank versionn 0.1.0.0.")

    describe "Get tune" $ do
      it "should get a tune (without need for authorization)" $ do
        result <- runClientM (tune Scandi augustssonId) clientEnv
        (second Meta.rhythm result) `shouldBe` (Right  "Engelska")

    describe "Get tune ABC" $ do
      it "should get the tune ABC text" $ do
        result <- runClientM (tuneAbc Scandi augustssonId) clientEnv
        (second (take 3 . unpack) result) `shouldBe` (Right  "X:1")

    describe "Get tunes" $ do
      it "should get all tunes from the genre" $ do
        eResult <- runClientM
                    (tuneList Scandi Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
                    clientEnv
        case eResult of
          Left _ ->
            expectationFailure "unexpected tuneList error"
          Right tList -> do
            (length $ tunes tList) `shouldBe` 3

    describe "POST tune" $ do
      it "should accept a new tune " $ do
        result <- runClientM (newTune normalUser Scandi (Submission augustsson)) clientEnv
        result `shouldBe` (Right augustssonId)

    describe "DELETE tune" $ do
      it "is allowed by an administrator" $ do
        result <- runClientM (deleteTune admin Scandi augustssonId) clientEnv
        result `shouldBe` (Right ())
      it "is barred for a normal user who didn't submit the tune" $ do
        eresult <- runClientM (deleteTune normalUser Scandi augustssonId) clientEnv
        case eresult of
          Left _ ->
            (isLeft eresult) `shouldBe` True
          Right _ -> do
            expectationFailure "unexpected deletion by non-owner"



-- MIME types for content negotiation
textPlain :: Maybe AcceptMime
textPlain =
  Just $ AcceptMime "text/plain"

textAbc :: Maybe AcceptMime
textAbc =
  Just $ AcceptMime "text/vnd.abc"

applicationJson :: Maybe AcceptMime
applicationJson =
  Just $ AcceptMime "application/json"
