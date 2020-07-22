module Main where

import Network.Wai.Handler.Warp

import qualified App
import           Server (mkApp)
import qualified Interview.Database as DB

main :: IO ()
main = do
    cfg <- App.getAppConfig
    DB.ensureNoPendingMigrations $ App.appDbConn cfg
    DB.runSeeds $ App.appDbConn cfg

    serverApp <- mkApp cfg
    run (App.appPort cfg) serverApp
