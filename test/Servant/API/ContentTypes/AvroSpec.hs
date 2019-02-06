{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Servant.API.ContentTypes.AvroSpec (spec) where

import           Control.Concurrent            (forkIO)
import           Control.Monad                 (void)
import           Control.Monad.IO.Class        (liftIO)
import           Data.Proxy                    (Proxy (..))
import           Data.Text                     (Text, pack)
import           Network.HTTP.Client           (defaultManagerSettings,
                                                newManager)
import qualified Network.Wai.Handler.Warp      as Warp
import           Servant.API
import           Servant.API.ContentTypes.Avro
import           Servant.Client
import           Servant.Server

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic



newtype ArbitraryText = ArbitraryText Text deriving Show

instance Arbitrary ArbitraryText where
  arbitrary = ArbitraryText . pack <$> arbitrary



type TestAPI =
  "put" :> ReqBody '[Avro, JSON] Text :> Put '[JSON] Text
  :<|>
  "get" :> Get '[Avro] (Maybe Text)

testApp :: Application
testApp = serve (Proxy :: Proxy TestAPI) handlers
  where
    handlers = handlePut :<|> handleGet

    handlePut :: Text -> Handler Text
    handlePut input = return $ input <> "x"

    handleGet :: Handler (Maybe Text)
    handleGet = return (Just "weewoo")

-- The app is stateless so we don't need a fresh server each time we run a test.
startTestApp :: IO ()
startTestApp = void . liftIO . forkIO $ Warp.run 8888 testApp

testPut :: Text -> ClientM Text
testGet :: ClientM (Maybe Text)
testPut :<|> testGet = client (Proxy :: Proxy TestAPI)


spec :: Spec
spec = do
  runIO startTestApp

  clientEnv <- runIO (mkClientEnv
                      <$> newManager defaultManagerSettings
                      <*> parseBaseUrl "http://localhost:8888")

  describe "GET /get" $
    it "produces the expected result" $ do
      result <- runClientM testGet clientEnv
      result `shouldBe` Right (Just "weewoo")

  describe "PUT /put" $
    it "appends 'x' to the request body and returns it" $ property $
      \(ArbitraryText str) -> monadicIO $ do
        result <- run $ runClientM (testPut str) clientEnv
        return $ result === Right (str <> "x")

  it "compiles" $
    True `shouldBe` True
