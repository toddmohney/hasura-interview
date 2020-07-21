{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server
  ( API
  , app
  , api
  , server
  ) where

import Network.Wai as Wai
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant

import API (API, api, routeHandlers)
import App

server :: ServerT API AppT
server = routeHandlers

app
    :: AppConfig
    -> Wai.Application
app cfg =
    logStdoutDev
        . corsMiddleWare
        $ serveWithContext api ctx serverT
  where
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

