{-# LANGUAGE OverloadedStrings, TypeFamilies, DataKinds,
  DeriveGeneric, TypeOperators #-}

module ApiTests (apiSpec) where


import           Prelude ()
import           Prelude.Compat

import qualified Control.Concurrent               as C
import           Control.Concurrent.MVar
import           Control.Exception                (bracket)
import           Control.Lens              hiding (Context)
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.HashMap.Strict              as HM
import           Data.Text                        (Text, unpack)
import           GHC.Generics
import           Network.HTTP.Client       hiding (Proxy)
import           Network.HTTP.Types
import           Network.Wai
import qualified Network.Wai.Handler.Warp         as Warp

import           Servant
import           Servant.Client
import           Servant.Server
import           Servant.QuickCheck
import           Servant.QuickCheck.Internal (serverDoesntSatisfy)

import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.Matcher


import Tunebank.ApiType (UserAPI)
import Tunebank.Server (userApp)
import Tunebank.Model.User

spec :: Spec
spec = do
  apiSpec


withUserApp :: IO () -> IO ()
withUserApp action =
  -- we can spin up a server in another thread and kill that thread when done
  -- in an exception-safe way
  bracket (liftIO $ C.forkIO $ Warp.run 8888 userApp)
    C.killThread
    (const action)


apiSpec :: Spec
apiSpec =
  -- `around` will start our Server before the tests and turn it off after
  around_ withUserApp $ do
    -- create a test client function
    let createUser = client (Proxy :: Proxy UserAPI)
    -- create a servant-client ClientEnv
    baseUrl <- runIO $ parseBaseUrl "http://localhost:8888"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv = mkClientEnv manager baseUrl

    describe "The test suite" $ do
      it "check 1 and 1 is 2" $ do
        let
          result = 1 + 1
        result `shouldBe` 2
