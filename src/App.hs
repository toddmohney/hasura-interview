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
import           Servant (Handler, ServerError)
import qualified System.Log.FastLogger as Log

import           AppConfig (AppConfig(..), getAppConfig)
import           Interview.Class.FastLogger (FastLogger(..), LogLevel(..), logMessage)
import           Interview.Class.Time (MonadTime(..))
import           Interview.Database (SqlPersistT, runSqlPool)
import           Interview.Database.Class (MonadDB(..))
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


instance MonadDB AppT where
    runDb
        :: SqlPersistT IO a
        -> AppT a
    runDb query = do
        conn <- asks appDbConn
        liftIO $ runSqlPool query conn


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
