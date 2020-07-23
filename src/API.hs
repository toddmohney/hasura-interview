{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module API where

import           Servant
import           Servant.HTML.Blaze (HTML)

import           App (AppT)
import           Interview.Orphans (WebSocket)
import           Pages.Home (HomePage)
import           RouteHandlers.Home (home)
import           RouteHandlers.Health (serverHealth)


type API =
    Home
        :<|> ServerHealth
        :<|> PublicAssets


type Home =
    Get '[HTML] HomePage


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
            :<|> serverHealth
            :<|> serveDirectoryWebApp "/usr/local/src/workspace/public"
