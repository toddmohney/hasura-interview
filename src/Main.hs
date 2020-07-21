module Main where

import Network.Wai.Handler.Warp

import qualified App
import           Server (app)
import qualified Interview.Database as DB

main :: IO ()
main = do
    cfg <- App.getAppConfig
    DB.ensureNoPendingMigrations $ App.appDbConn cfg
    DB.runSeeds $ App.appDbConn cfg
    run (App.appPort cfg) (app cfg)
