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
import Data.Bifunctor (second)
import Data.Genre (Genre(..))

import Servant
import Servant.Client

import Test.Hspec
import Test.Hspec.Wai

import Data.ByteString.Lazy (ByteString)

import Tunebank.ApiType (AbcTuneAPI1)
import Tunebank.Types
import Tunebank.Server (tuneApp)
import Tunebank.Model.User
import Tunebank.Model.NewTune
import Tunebank.Model.TuneRef
import qualified Tunebank.Model.AbcMetadata as Meta
import Data.Configurator.Types (Config)
import TestData


tune ::  Genre -> TuneId -> ClientM Meta.AbcMetadata
tunePdf  ::  Genre -> TuneId -> ClientM ByteString
tunePs   ::  Genre -> TuneId -> ClientM ByteString
tunePng  ::  Genre -> TuneId -> ClientM ByteString
tuneMidi ::  Genre -> TuneId -> ClientM ByteString
tunes ::  Genre
       -> Maybe Meta.Title
       -> Maybe Meta.Rhythm
       -> Maybe Meta.TuneKey
       -> Maybe Meta.Source
       -> Maybe Meta.Origin
       -> Maybe Meta.Composer
       -> Maybe Meta.Transcriber
       -> ClientM [TuneRef]
newTune :: BasicAuthData -> Genre -> Submission -> ClientM TuneId
tune :<|> tunePdf :<|> tunePs :<|> tunePng :<|> tuneMidi
      :<|> tunes :<|> newTune = client (Proxy :: Proxy AbcTuneAPI1)

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

    describe "Get tune" $ do
      it "should get a tune (without need for authorization)" $ do
        result <- runClientM (tune Scandi augustssonId) clientEnv
        (second Meta.rhythm result) `shouldBe` (Right  "Engelska")

    describe "Get tunes" $ do
      it "should get all tunes from the genre" $ do
        result <- runClientM
                    (tunes Scandi Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
                    clientEnv
        (second length result) `shouldBe` (Right 3)

    describe "POST tune" $ do
      it "should accept a new tune " $ do
        result <- runClientM (newTune normalUser Scandi (Submission $ pack augustsson)) clientEnv
        result `shouldBe` (Right augustssonId)
