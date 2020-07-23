{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server
  ( API
  , mkApp
  , api
  , server
  ) where

import Network.Wai as Wai
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant
import qualified Servant.Ekg as EKG

import API (API, api, routeHandlers)
import App

server :: ServerT API AppT
server = routeHandlers

mkApp
    :: AppConfig
    -> IO Wai.Application
mkApp cfg = do
    monitorEndpoints' <- EKG.monitorEndpoints api (metricsStore cfg)
    pure $ monitorEndpoints' app
  where
    app =
        logStdoutDev
            . corsMiddleWare
            $ serveWithContext api ctx serverT

    serverT = hoistServerWithContext
        api
        (Proxy :: Proxy '[])
        (nt cfg)
        server

    ctx = EmptyContext


nt :: AppConfig -> AppT a -> Handler a
nt cfg x = App.runAppT cfg x


corsMiddleWare :: Middleware
corsMiddleWare = cors (const $ Just corsPolicy)


corsPolicy :: CorsResourcePolicy
corsPolicy =
    let allowedMethods = simpleMethods <> ["DELETE", "PUT", "OPTIONS"]
        allowedHeaders = ["Content-Type"]
    in
        simpleCorsResourcePolicy { corsOrigins = Nothing
                                 , corsMethods = allowedMethods
                                 , corsRequestHeaders = allowedHeaders
                                 }


