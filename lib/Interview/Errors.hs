module Interview.Errors
    ( PendingMigrationsError(..)
    ) where

import           Control.Monad.Catch (Exception)
import           Data.Text (Text)
import           GHC.Generics (Generic)


newtype PendingMigrationsError = PendingMigrationsError Text
    deriving (Show, Eq, Generic)

instance Exception PendingMigrationsError

