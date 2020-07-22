module Interview.Class.Concurrency where

import           Control.Concurrent (threadDelay)

newtype Seconds = Seconds Int
    deriving (Show, Eq, Ord, Num)

class (Monad m) => Concurrency m where
    sleep :: Seconds -> m ()


instance Concurrency IO where
    sleep :: Seconds -> IO ()
    sleep (Seconds secs) =
        threadDelay $ secs * 1000000
