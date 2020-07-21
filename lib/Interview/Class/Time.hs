module Interview.Class.Time
    ( MonadTime(..)
    , UTCTime
    , toDateString
    , TC.utcTimeToPOSIXSeconds
    ) where

import           Control.Monad.Except (ExceptT)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger (LoggingT)
import           Control.Monad.Reader (ReaderT)
import           Data.Int (Int64)
import           Data.Time.Clock (UTCTime)
import qualified Data.Time.Clock as TC
import qualified Data.Time.Clock.POSIX as TC
import           Data.Time.Clock.System (SystemTime)
import qualified Data.Time.Clock.System as TC
import qualified Data.Time.Format as TF

class (MonadIO m) => MonadTime m where
    getCurrentTime :: m UTCTime
    default getCurrentTime :: m UTCTime
    getCurrentTime = liftIO getCurrentTime

    getSystemTime :: m SystemTime
    default getSystemTime :: m SystemTime
    getSystemTime = liftIO getSystemTime

    getEpochTime :: m Int64
    default getEpochTime :: m Int64
    getEpochTime = TC.systemSeconds <$> getSystemTime

instance (MonadIO m, MonadTime m) => MonadTime (ExceptT e m)
instance (MonadIO m, MonadTime m) => MonadTime (ReaderT s m)
instance (MonadIO m, MonadTime m) => MonadTime (LoggingT m)

instance MonadTime IO where
    getCurrentTime = TC.getCurrentTime

    getSystemTime = TC.getSystemTime

    getEpochTime = TC.systemSeconds <$> getSystemTime

toDateString
    :: UTCTime
    -> String
toDateString =
    TF.formatTime TF.defaultTimeLocale "%D"

