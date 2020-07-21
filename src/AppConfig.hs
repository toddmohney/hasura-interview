module AppConfig
  ( AppConfig(..)
  , getAppConfig
  ) where

import           System.Environment (getEnv, lookupEnv)

import           Interview.Class.FastLogger (LoggerSet)
import qualified Interview.Class.FastLogger as Log
import           Interview.Database (ConnectionPool, mkPool)
import           Interview.Environment

data AppConfig = AppConfig
    { appEnv :: !Environment
    , appPort :: !Int
    , appLogger :: !LoggerSet
    , appDebugging :: !Bool
    , appDbConn :: !ConnectionPool
    }

{-
 - Load all external configuration at application startup time.
 - This allows us to fail fast in the case where our server is misconfigured.
 -}
getAppConfig :: IO AppConfig
getAppConfig = do
    env <- read <$> getEnv "ENV"
    port <- read <$> getEnv "PORT"
    logger <- mkLoggerSet
    debugging <- maybe False read <$> lookupEnv "DEBUG"
    dbPool <- mkPool env

    pure $ AppConfig
        { appEnv = env
        , appPort = port
        , appLogger = logger
        , appDebugging = debugging
        , appDbConn = dbPool
        }


mkLoggerSet :: IO LoggerSet
mkLoggerSet = Log.newStdoutLoggerSet Log.defaultBufSize
