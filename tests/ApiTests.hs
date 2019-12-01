{-# LANGUAGE OverloadedStrings, TypeFamilies, DataKinds,
  DeriveGeneric, TypeOperators #-}


{- The types mean that you don't need to test:

     What your endpoints return
     Your URL structure
     What your endpoints take

     instead, just test the business logic by going directly to the handler
-}

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

import           Control.Error.Util (hush)
import           Data.Either (isLeft)
import           Data.Bifunctor (first, second)

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
import qualified Tunebank.Model.UserRegistration as UReg (Submission(..))

sampleNewUser :: UReg.Submission
sampleNewUser = UReg.Submission "fred" "fred@gmail.com" "pwd" "pwd" "http://localhost"

admin :: BasicAuthData
admin =
   BasicAuthData "Administrator" "password"

badUser :: BasicAuthData
badUser =
   BasicAuthData "Joe" "wibble"

spec :: Spec
spec = do
  apiSpec

users :: BasicAuthData -> ClientM [User]
newUser :: UReg.Submission -> ClientM User
checkUser :: BasicAuthData -> ClientM Text
users :<|> newUser :<|> checkUser = client (Proxy :: Proxy UserAPI)


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
    -- create a servant-client ClientEnv
    baseUrl <- runIO $ parseBaseUrl "http://localhost:8888"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv = mkClientEnv manager baseUrl

    describe "POST user" $ do
      it "should create a user " $ do
        result <- runClientM (newUser sampleNewUser) clientEnv
        (second name result) `shouldBe` (Right  "fred")
        (second registered result)  `shouldBe` (Right False)

    describe "GET users" $ do
      it "should get a user list " $ do
        result <- runClientM  (users admin) clientEnv
        (second length result) `shouldBe` (Right 3)

    describe "check user" $ do
      it "should accept a valid user " $ do
        result <- runClientM  (checkUser admin) clientEnv
        result `shouldBe` (Right "Y")
      it "should reject an invalid user " $ do
        result <- runClientM  (checkUser badUser) clientEnv
        -- maybe we aught to analyse the error but this shoule be enough
        (isLeft result) `shouldBe` True
