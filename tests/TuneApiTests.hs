{-# LANGUAGE OverloadedStrings, TypeFamilies, DataKinds,
  DeriveGeneric, TypeOperators #-}


module TuneApiTests (tuneApiSpec) where

import Prelude ()
import Prelude.Compat

import qualified Control.Concurrent as C
import Control.Monad.Reader
import Data.IORef (IORef, newIORef)
import Control.Exception (bracket)
import Data.Text (Text, unpack)
import Network.HTTP.Client hiding (Proxy)
import qualified Network.Wai.Handler.Warp as Warp

import Data.Either (isLeft, isRight)
import Data.Bifunctor (second)
import Data.Genre (Genre(..))
import Servant
import Servant.Client

import Test.Hspec

import Data.ByteString.Lazy (ByteString)

import Tunebank.ApiType (AbcTuneAPI)
import Tunebank.Types
import Tunebank.Server (tuneAPI, tuneServer)
import Tunebank.Model.User
import Tunebank.Model.TuneText
import Tunebank.Model.TuneRef
import qualified Tunebank.Model.AbcMetadata as Meta
import Data.Configurator.Types (Config)
import TestData
import qualified Mock.DBState as MockDB
import qualified Mock.MockBasicAuth as MockAuth (basicAuthServerContext)

fixtureDelay :: Int
fixtureDelay =
  -- 100 ms
  100000


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
newTune :: BasicAuthData -> Genre -> Submission -> ClientM Text
deleteTune :: BasicAuthData -> Genre ->  TuneId -> ClientM ()
welcome :<|> tune :<|> tunePdf :<|> tunePs :<|> tunePng
      :<|> tuneMidi :<|> tuneAbc
      :<|> tuneList :<|> newTune
      :<|> deleteTune = client (Proxy :: Proxy AbcTuneAPI)


tuneApp :: IORef MockDB.DBState ->  AppCtx -> Application
tuneApp dbRef ctx =
  serveWithContext tuneAPI MockAuth.basicAuthServerContext $
    hoistServerWithContext tuneAPI (Proxy :: Proxy (BasicAuthCheck UserName ': '[]))
      (flip runReaderT ctx) (tuneServer $ MockDB.DBIORef dbRef)

withTuneApp :: Config -> IO () -> IO ()
withTuneApp config action = do
  dbRef <- newIORef MockDB.mockedDBState
  -- we can spin up a server in another thread and kill that thread when done
  -- in an exception-safe way
  bracket (do
            -- let's try a sandwich
            _ <- liftIO $ C.threadDelay fixtureDelay
            thread <- liftIO $ C.forkIO $ Warp.run 8888 (tuneApp dbRef $ AppCtx config)
            _ <- liftIO $ C.threadDelay fixtureDelay
            pure thread
           )
    C.killThread
    (const action)

tuneApiSpec :: Config -> Spec
tuneApiSpec config =
  -- `around` will start our Server before the tests and turn it off after
  around_ (withTuneApp config) $ do
    base <- runIO $ parseBaseUrl "http://localhost:8888"
    mgr <- runIO $ newManager defaultManagerSettings
    let clientEnv = mkClientEnv mgr base

    describe "Welcome" $ do
      it "should provide a wecome message" $ do
        result <- runClientM welcome clientEnv
        result `shouldBe` (Right  "Welcome to tunebank version 0.1.0.0.")

    describe "Get tune" $ do
      it "should get a tune (without need for authorization)" $ do
        result <- runClientM (tune Scandi augustssonId) clientEnv
        (second Meta.rhythm result) `shouldBe` (Right  "Engelska")

    describe "Get tune ABC" $ do
      it "should get the tune ABC text" $ do
        result <- runClientM (tuneAbc Scandi augustssonId) clientEnv
        (second (take 3 . unpack) result) `shouldBe` (Right  "X:1")

    describe "Get tune PDF" $ do
      it "should get the tune in PDF format" $ do
        result <- runClientM (tunePdf Scandi augustssonId) clientEnv
        (isRight result) `shouldBe` True

    describe "Get tune MIDI" $ do
      it "should get the tune in MIDI format" $ do
        result <- runClientM (tuneMidi Scandi augustssonId) clientEnv
        (isRight result) `shouldBe` True

    describe "Get tune PNG" $ do
      it "should get the tune in PNG format" $ do
        result <- runClientM (tunePng Scandi andetBrudstykkeId) clientEnv
        (isRight result) `shouldBe` True

    describe "Get tune PostScript" $ do
      it "should get the tune in PostScript format" $ do
        result <- runClientM (tunePs Scandi andetBrudstykkeId) clientEnv
        (isRight result) `shouldBe` True

    describe "get tune list (search)" $ do
      it "should get all tunes from the genre" $ do
        eResult <- runClientM
                    (tuneList Scandi Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just 1) (Just 15))
                    clientEnv
        case eResult of
          Left err ->
            expectationFailure ("unexpected tuneList error: " <> show err)
          Right tList -> do
            (length $ tunes tList) `shouldBe` 4

    describe "POST tune" $ do
      it "should accept a completely new tune " $ do
        result <- runClientM (newTune normalUserFred Scandi (Submission ewa)) clientEnv
        result `shouldBe` (Right "ewa-polska")

      it "should reject a submission of a new tune if was already submitted by previous user " $ do
        result <- runClientM (newTune normalUserFred Scandi (Submission augustsson)) clientEnv
        (isLeft result) `shouldBe` True

    describe "DELETE tune" $ do
      it "is allowed by an administrator" $ do
        result <- runClientM (deleteTune admin Scandi augustssonId) clientEnv
        result `shouldBe` (Right ())
      it "is barred for a normal user who didn't submit the tune" $ do
        eresult <- runClientM (deleteTune normalUserFred Scandi augustssonId) clientEnv
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
