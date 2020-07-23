{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Servant.EkgSpec (spec) where

import           Data.Aeson
import qualified Data.HashMap.Strict                        as H
import           Data.Monoid                                ((<>))
import           Data.Proxy
import           Data.Text
import           GHC.Generics
import           Network.HTTP.Client                        (defaultManagerSettings,
                                                             newManager)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Client
#if MIN_VERSION_servant(0,15,0)
import           Servant.Test.ComprehensiveAPI              (comprehensiveAPI)
#else
import           Servant.API.Internal.Test.ComprehensiveAPI (comprehensiveAPI)
#endif
import           System.Metrics
import           Test.Hspec

import           Servant.Ekg

#if !MIN_VERSION_servant_client(0,16,0)
#define ClientError ServantError
#endif

-- * Spec

spec :: Spec
spec = describe "servant-ekg" $ do

  let getEp :<|> postEp :<|> deleteEp = client testApi

  it "collects number of request" $
    withApp $ \port store -> do
      mgr <- newManager defaultManagerSettings
      let runFn :: ClientM a -> IO (Either ClientError a)
          runFn fn = runClientM fn (mkClientEnv mgr (BaseUrl Http "localhost" port ""))
      _ <- runFn $ getEp "name" Nothing
      _ <- runFn $ postEp (Greet "hi")
      _ <- runFn $ deleteEp "blah"

      m <- sampleAll store
      case H.lookup "servant.path.hello.:name.GET.responses.2XX" m of
        Nothing -> fail "Expected some value"
        Just v  -> v `shouldBe` Counter 1
      case H.lookup "servant.path.greet.POST.responses.2XX" m of
        Nothing -> fail "Expected some value"
        Just v  -> v `shouldBe` Counter 1
      case H.lookup "servant.path.greet.:greetid.DELETE.responses.2XX" m of
        Nothing -> fail "Expected some value"
        Just v  -> v `shouldBe` Counter 1

  it "is comprehensive" $ do
    _typeLevelTest <- monitorEndpoints comprehensiveAPI =<< newStore
    True `shouldBe` True

  it "enumerates the parts of an API correctly" $
    enumerateEndpoints testApi `shouldBe` [
      APIEndpoint ["hello",":name"] "GET",
      APIEndpoint ["greet"] "POST",
      APIEndpoint ["greet",":greetid"] "DELETE"
    ]

-- * Example

-- | A greet message data type
newtype Greet = Greet { _msg :: Text }
  deriving (Generic, Show)

instance FromJSON Greet
instance ToJSON Greet

-- API specification
type TestApi =
       -- GET /hello/:name?capital={true, false}  returns a Greet as JSON
       "hello" :> Capture "name" Text :> QueryParam "capital" Bool :> Get '[JSON] Greet

       -- POST /greet with a Greet as JSON in the request body,
       --             returns a Greet as JSON
  :<|> "greet" :> ReqBody '[JSON] Greet :> Post '[JSON] Greet

       -- DELETE /greet/:greetid
  :<|> "greet" :> Capture "greetid" Text :> Delete '[JSON] NoContent

testApi :: Proxy TestApi
testApi = Proxy

-- Server-side handlers.
--
-- There's one handler per endpoint, which, just like in the type
-- that represents the API, are glued together using :<|>.
--
-- Each handler runs in the 'EitherT (Int, String) IO' monad.
server :: Server TestApi
server = helloH :<|> postGreetH :<|> deleteGreetH

  where helloH name Nothing = helloH name (Just False)
        helloH name (Just False) = return . Greet $ "Hello, " <> name
        helloH name (Just True) = return . Greet . toUpper $ "Hello, " <> name

        postGreetH = return

        deleteGreetH _ = return NoContent

-- Turn the server into a WAI app. 'serve' is provided by servant,
-- more precisely by the Servant.Server module.
test :: Application
test = serve testApi server

withApp :: (Port -> Store -> IO a) -> IO a
withApp a = do
  ekg <- newStore
  monitorEndpoints' <- monitorEndpoints testApi ekg
  withApplication (return $ monitorEndpoints' test) $ \p -> a p ekg
