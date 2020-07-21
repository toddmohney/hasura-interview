module Interview.Class.FastLogger
    ( FastLogger(..)
    , LoggerSet
    , LogLevel(..)
    , LogMessage(..)
    , logMessage
    , Log.newStdoutLoggerSet
    , Log.defaultBufSize
    ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Time.Format as Time
import           GHC.Generics (Generic)
import           System.Log.FastLogger (LoggerSet, ToLogStr(..))
import qualified System.Log.FastLogger as Log

import Interview.Class.Time (UTCTime, getCurrentTime)

class (MonadIO m) => FastLogger m where
    logDebug :: Text -> m ()
    logInfo :: Text -> m ()
    logWarn :: Text -> m ()
    logError :: Text -> m ()
    flushLogs :: m ()

instance FastLogger IO where
    logDebug :: Text -> IO ()
    logDebug msg = do
        logger <- Log.newStdoutLoggerSet Log.defaultBufSize
        logMessage logger msg DEBUG

    logInfo :: Text -> IO ()
    logInfo msg = do
        logger <- Log.newStdoutLoggerSet Log.defaultBufSize
        logMessage logger msg INFO

    logWarn :: Text -> IO ()
    logWarn msg = do
        logger <- Log.newStdoutLoggerSet Log.defaultBufSize
        logMessage logger msg WARN

    logError :: Text -> IO ()
    logError msg = do
        logger <- Log.newStdoutLoggerSet Log.defaultBufSize
        logMessage logger msg ERROR

    flushLogs :: IO ()
    flushLogs = pure ()


logMessage :: (MonadIO m) => LoggerSet -> Text -> LogLevel -> m ()
logMessage logger msg lvl = do
    now <- liftIO getCurrentTime
    let logMsg = LogMessage
                    { message = msg
                    , timestamp = now
                    , level = lvl
                    }
    liftIO $ Log.pushLogStrLn logger (Log.toLogStr logMsg)

data LogLevel = DEBUG
              | INFO
              | WARN
              | ERROR
    deriving (Show, Eq)

data LogMessage = LogMessage
    { message   :: !Text
    , timestamp :: !UTCTime
    , level     :: !LogLevel
    } deriving (Eq, Show, Generic)

instance ToLogStr LogMessage where
    toLogStr = Log.toLogStr . formatMessage
      where
        formatMessage :: LogMessage -> Text
        formatMessage msg =
            formatTimestamp msg
                <> " "
                <> T.pack ("[" <> show (level msg) <> "]")
                <> " "
                <> message msg

        formatTimestamp msg =
            T.pack $ Time.formatTime Time.defaultTimeLocale (Time.iso8601DateFormat (Just "%H:%M:%S")) (timestamp msg)


