{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module API where

import           Servant
import           Servant.HTML.Blaze (HTML)

import           App (AppT)
import           Pages.Home (HomePage)
import           RouteHandlers.Home (home)


type API =
    Home
        :<|> PublicAssets


type Home =
    Get '[HTML] HomePage


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
            :<|> serveDirectoryWebApp "/usr/local/src/interview/assets/dist"
