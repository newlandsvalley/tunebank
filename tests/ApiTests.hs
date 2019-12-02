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
import           Control.Exception                (bracket)
import           Data.Text                        (Text, unpack)
import           Network.HTTP.Client       hiding (Proxy)
import qualified Network.Wai.Handler.Warp         as Warp

-- import           Control.Error.Util (hush)
import           Data.Either (isLeft)
import           Data.Bifunctor (second)

import           Servant
import           Servant.Client
-- import           Servant.QuickCheck
-- import           Servant.QuickCheck.Internal (serverDoesntSatisfy)

import           Test.Hspec
import           Test.Hspec.Wai
-- import           Test.Hspec.Wai.Matcher


import Tunebank.ApiType (UserAPI)
import Tunebank.Server (userApp)
import Tunebank.Model.User
import qualified Tunebank.Model.UserRegistration as UReg (Submission(..))

sampleNewUser :: UReg.Submission
sampleNewUser = UReg.Submission "fred" "fred@gmail.com" "pwd" "pwd" "http://localhost"

admin :: BasicAuthData
admin =
   BasicAuthData "Administrator" "password"

normalUser :: BasicAuthData
normalUser =
  BasicAuthData "Fred" "password"

badUser :: BasicAuthData
badUser =
   BasicAuthData "Joe" "wibble"

validateableUid :: UserId
validateableUid =
  UserId "FRED"

{-}
spec :: Spec
spec = do
  apiSpec
-}

users :: BasicAuthData -> ClientM [User]
newUser :: UReg.Submission -> ClientM User
checkUser :: BasicAuthData -> ClientM Text
validateUser :: UserId -> ClientM Text
users :<|> newUser :<|> checkUser :<|> validateUser = client (Proxy :: Proxy UserAPI)


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
    base <- runIO $ parseBaseUrl "http://localhost:8888"
    mgr <- runIO $ newManager defaultManagerSettings
    let clientEnv = mkClientEnv mgr base

    describe "POST user" $ do
      it "should create a user " $ do
        result <- runClientM (newUser sampleNewUser) clientEnv
        (second name result) `shouldBe` (Right  "fred")
        (second registered result)  `shouldBe` (Right False)

    describe "GET users" $ do
      it "should get a user list " $ do
        result <- runClientM  (users admin) clientEnv
        (second length result) `shouldBe` (Right 4)
      it "should reject a non-admin (normal) user auth" $ do
        result <- runClientM  (users normalUser) clientEnv
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
        result <- runClientM  (validateUser (UserId "WIBBLE")) clientEnv
        (isLeft result) `shouldBe` True
