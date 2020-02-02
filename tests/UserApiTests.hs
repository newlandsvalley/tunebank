{-# LANGUAGE OverloadedStrings, TypeFamilies, DataKinds,
  DeriveGeneric, TypeOperators #-}


{- The types mean that you don't need to test:

     What your endpoints return
     Your URL structure
     What your endpoints take

     instead, just test the business logic by going directly to the handler
-}

module UserApiTests (userApiSpec) where


import           Prelude ()
import           Prelude.Compat

import qualified Control.Concurrent               as C
import           Control.Exception                (bracket)
import           Data.Text                        (Text, unpack)
import           Network.HTTP.Client       hiding (Proxy)
import qualified Network.Wai.Handler.Warp         as Warp

import           Data.Either (isLeft)
import           Data.Bifunctor (second)

import           Servant
import           Servant.Client

import           Test.Hspec
import           Test.Hspec.Wai


import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Control.Monad.Reader

import Tunebank.ApiType (UserAPI)
import Tunebank.Types (AppCtx(..))
import Tunebank.Server (userAPI, userServer)
import Tunebank.Model.User
import qualified Tunebank.Model.UserRegistration as UReg (Submission(..))
import Tunebank.Authentication.BasicAuth (basicAuthServerContext)
import Data.Configurator.Types (Config)
import TestData
import qualified Mock.DBState as MockDB
import qualified Mock.MockBasicAuth as MockAuth (basicAuthServerContext)

userList :: BasicAuthData -> Maybe Int -> Maybe Int -> ClientM UserList
newUser :: UReg.Submission -> ClientM Text
checkUser :: BasicAuthData -> ClientM Text
validateUser :: UserId -> ClientM Text
userList :<|> newUser :<|> checkUser :<|> validateUser = client (Proxy :: Proxy UserAPI)


userApp :: IORef MockDB.DBState -> AppCtx -> Application
userApp dbRef ctx = do
  serveWithContext userAPI MockAuth.basicAuthServerContext $
    hoistServerWithContext userAPI (Proxy :: Proxy (BasicAuthCheck UserName ': '[]))
      (flip runReaderT ctx) (userServer $ MockDB.DBIORef dbRef)

withUserApp :: Config -> IO () -> IO ()
withUserApp config action = do
  dbRef <- newIORef MockDB.mockedDBState
  -- we can spin up a server in another thread and kill that thread when done
  -- in an exception-safe way
  bracket (liftIO $ C.forkIO $ Warp.run 8888 (userApp dbRef (AppCtx config)) )
    C.killThread
    (const action)



userApiSpec :: Config -> Spec
userApiSpec config =
  -- `around` will start our Server before the tests and turn it off after
  around_ (withUserApp config) $ do
    base <- runIO $ parseBaseUrl "http://localhost:8888"
    mgr <- runIO $ newManager defaultManagerSettings
    let clientEnv = mkClientEnv mgr base

    describe "POST user" $ do
      it "should create a user " $ do
        result <- runClientM (newUser sampleNewUser) clientEnv
        (second (take 52 . unpack) result) `shouldBe` (Right  "we've sent you an email to complete the registration")

    describe "GET users" $ do
      it "should get a user list " $ do
        eResult <- runClientM (userList admin Nothing Nothing) clientEnv
        case eResult of
          Left _ ->
            expectationFailure "unexpected userList error"
          Right uList -> do
            (length $ users uList) `shouldBe` 4
      it "should reject a non-admin (normal) user auth" $ do
        result <- runClientM  (userList normalUser Nothing Nothing) clientEnv
        (isLeft result) `shouldBe` True

    describe "check user" $ do
      it "should accept a valid user " $ do
        result <- runClientM  (checkUser admin) clientEnv
        result `shouldBe` (Right "Y")
      it "should reject an invalid user " $ do
        result <- runClientM  (checkUser badUser) clientEnv
        -- maybe we aught to analyse the error but this shoule be enough
        (isLeft result) `shouldBe` True

    describe "validate user registration" $ do
      it "should accept a valid user id slub" $ do
        result <- runClientM  (validateUser validateableUid) clientEnv
        result `shouldBe` (Right "Y")
      it "should reject an invalid user id slub" $ do
        result <- runClientM  (validateUser (UserId 9999)) clientEnv
        (isLeft result) `shouldBe` True
