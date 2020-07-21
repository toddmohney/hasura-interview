module Interview.Database.Migrations where


import Control.Monad (when)
import Control.Exception (throwIO)
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Postgresql
    ( runMigration
    , runMigrationSilent
    , showMigration
    )

import Interview.Database.Models
import Interview.Errors (PendingMigrationsError(..))

runMigrations :: ConnectionPool -> IO ()
runMigrations = runSqlPool (runMigration migrateAll)


runMigrations' :: ConnectionPool -> IO [Text]
runMigrations' = runSqlPool (runMigrationSilent migrateAll)


ensureNoPendingMigrations :: ConnectionPool -> IO ()
ensureNoPendingMigrations connPool = do
    results <- runSqlPool (showMigration migrateAll) connPool
    when (not $ L.null results) $ do
        putStrLn . T.unpack $ mkMessage results
        throwIO $ PendingMigrationsError ""
  where
    mkMessage migrations =
        T.unlines $
            [ "/////////////////////////////////////////////////"
            , ""
            , "There are pending migrations."
            , "Please implement the following statements using our database migration software"
            , ""
            ]
            <> migrations
            <>
            [ ""
            , "/////////////////////////////////////////////////"
            ]


runSeeds :: ConnectionPool -> IO ()
runSeeds conn =
    flip runSqlPool conn $ do
        -- Put your database seed instructions here
        pure ()

