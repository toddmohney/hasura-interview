module Interview.Database.Class where

import           Interview.Database.Models

class (Monad m) => MonadDB m where
    runDb
        :: SqlPersistT IO a
        -> m a
