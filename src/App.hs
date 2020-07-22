{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module App
    ( AppT
    , AppConfig(..)
    , getAppConfig
    , runAppT
    ) where

import           Control.Monad (when)
import           Control.Monad.Catch (MonadCatch, MonadThrow)
import           Control.Monad.Except (MonadError(..))
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import           Data.Text (Text)
import           Network.WebSockets (WebSocketsData)
import           Network.WebSockets.Connection (Connection)
import           Servant (Handler, ServerError)
import qualified System.Log.FastLogger as Log
import           System.Metrics (Sample)
import qualified System.Metrics as EKG

import           AppConfig (AppConfig(..), getAppConfig)
import           Interview.Class.Concurrency (Concurrency(..), Seconds(..))
import           Interview.Class.FastLogger (FastLogger(..), LogLevel(..), logMessage)
import           Interview.Class.Instrumentation (Instrumentation(..))
import           Interview.Class.Time (MonadTime(..))
import           Interview.Class.WebSocket (PingInterval(..), WebSocket(..))
import           Interview.Environment (Environment(..))


newtype AppT a = AppT { unApp :: ReaderT AppConfig Handler a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadReader AppConfig
             , MonadError ServerError
             , MonadThrow
             , MonadCatch
             )

runAppT :: AppConfig -> AppT a -> Handler a
runAppT cfg appT = runReaderT (unApp appT) cfg


instance Concurrency AppT where
    sleep
        :: Seconds
        -> AppT ()
    sleep secs =
        liftIO $ sleep secs


instance Instrumentation AppT where
    sampleAll
        :: AppT Sample
    sampleAll = do
        store <- asks metricsStore
        liftIO $ EKG.sampleAll store


instance WebSocket AppT where
    forkPingThread
        :: Connection
        -> PingInterval
        -> AppT ()
    forkPingThread conn interval =
        liftIO $ forkPingThread conn interval


    sendTextDatas
        :: (WebSocketsData a)
        => Connection
        -> [a]
        -> AppT ()
    sendTextDatas conn msgs =
        liftIO $ sendTextDatas conn msgs


instance MonadTime AppT where
    getCurrentTime = liftIO getCurrentTime

    getSystemTime = liftIO getSystemTime

    getEpochTime = liftIO getEpochTime


instance FastLogger AppT where
    logDebug :: Text -> AppT ()
    logDebug msg = do
        env <- asks appEnv
        debugging <- asks appDebugging
        when (debugging || env == Development || env == Test) $ do
            logger <- asks appLogger
            logMessage logger msg DEBUG

    logInfo :: Text -> AppT ()
    logInfo msg = do
        logger <- asks appLogger
        logMessage logger msg INFO

    logWarn :: Text -> AppT ()
    logWarn msg = do
        logger <- asks appLogger
        logMessage logger msg WARN

    logError :: Text -> AppT ()
    logError msg = do
        logger <- asks appLogger
        logMessage logger msg ERROR

    flushLogs :: AppT ()
    flushLogs = do
        logger <- asks appLogger
        liftIO $ Log.flushLogStr logger
