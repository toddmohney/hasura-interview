module Main where

import Network.Wai.Handler.Warp

import qualified App
import           Server (mkApp)

main :: IO ()
main = do
    cfg <- App.getAppConfig
    serverApp <- mkApp cfg
    run (App.appPort cfg) serverApp
