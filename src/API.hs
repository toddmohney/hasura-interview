{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module API where

import           Servant
import           Servant.HTML.Blaze (HTML)

import           App (AppT)
import           Interview.Orphans (WebSocket)
import           Pages.Home (HomePage, HelloPage)
import           RouteHandlers.Home (home, hello)
import           RouteHandlers.Health (serverHealth)


type API =
    Home
        :<|> Hello
        :<|> ServerHealth
        :<|> PublicAssets


type Home =
    Get '[HTML] HomePage


type Hello =
    "hello"
        :> Get '[HTML] HelloPage


type ServerHealth =
    "health"
        :> "status"
        :> WebSocket


type PublicAssets =
    "public" :> Raw


api :: Proxy API
api = Proxy


routeHandlers :: ServerT API AppT
routeHandlers =
    publicRouteHandlers
  where
    publicRouteHandlers =
        home
            :<|> hello
            :<|> serverHealth
            :<|> serveDirectoryWebApp "/usr/local/src/workspace/public"
