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

import Tunebank.ApiType (AbcTuneAPI1)
import Tunebank.Server (tuneApp)
import Tunebank.Model.User
import Tunebank.Model.NewTune
import Tunebank.Model.TuneRef
import Tunebank.Model.AbcMetadata
import TestData


tune ::  Genre -> TuneId -> ClientM AbcMetadata
tunes ::  Genre ->  ClientM [TuneRef]
newTune :: BasicAuthData -> Genre -> Submission -> ClientM TuneId
tune :<|> tunes :<|> newTune = client (Proxy :: Proxy AbcTuneAPI1)

withUserApp :: IO () -> IO ()
withUserApp action =
  -- we can spin up a server in another thread and kill that thread when done
  -- in an exception-safe way
  bracket (liftIO $ C.forkIO $ Warp.run 8888 tuneApp)
    C.killThread
    (const action)

tuneApiSpec :: Spec
tuneApiSpec =
  -- `around` will start our Server before the tests and turn it off after
  around_ withUserApp $ do
    base <- runIO $ parseBaseUrl "http://localhost:8888"
    mgr <- runIO $ newManager defaultManagerSettings
    let clientEnv = mkClientEnv mgr base

    describe "POST tune" $ do
      it "should accept a new tune " $ do
        result <- runClientM (newTune admin Scandi (Submission $ pack augustsson)) clientEnv
        result `shouldBe` (Right $ TuneId "engelska efter albert augustsson-engelska")
