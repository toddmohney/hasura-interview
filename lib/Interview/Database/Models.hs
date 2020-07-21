{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Interview.Database.Models
    ( module Interview.Database.Models
    , ConnectionPool
    , ConnectionString
    , Entity(..)
    , Filter(..)
    , Key
    , SelectOpt(..)
    , SqlPersistT
    , Unique
    , Update(..)
    , PersistUpdate(..)
    , rawExecute
    , runSqlPool
    , count
    , deleteWhere
    , get
    , getBy
    , getEntity
    , insert
    , insert_
    , insertEntity
    , insertUnique
    , putMany
    , replace
    , selectFirst
    , selectList
    , update
    , updateGet
    , updateWhere
    , upsert
    , (==.)
    , (||.)
    , (=.)
    , (!=.)
    ) where

import Database.Persist.Postgresql
    ( ConnectionPool
    , ConnectionString
    , Entity(..)
    , SqlPersistT
    , rawExecute
    , runSqlPool
    )
import Database.Persist.Sql
    ( PersistUpdate(..)
    , Filter(..)
    , Key
    , SelectOpt(..)
    , SqlBackend
    , ToBackendKey
    , Unique
    , Update(..)
    , deleteWhere
    , fromSqlKey
    , count
    , get
    , getBy
    , getEntity
    , insert
    , insertEntity
    , insertUnique
    , insert_
    , putMany
    , replace
    , selectFirst
    , selectList
    , toSqlKey
    , update
    , updateGet
    , updateWhere
    , upsert
    , (=.)
    , (==.)
    , (||.)
    , (!=.)
    )
import Database.Persist.TH


share [mkPersist sqlSettings { mpsGenerateLenses = True }, mkMigrate "migrateAll"] [persistLowerCase|
|]


toKey
    :: ( Integral i
       , ToBackendKey SqlBackend record
       )
    => i
    -> Key record
toKey =
    toSqlKey . fromIntegral


fromKey
    :: ( Integral i
       , ToBackendKey SqlBackend record
       )
    => Key record
    -> i
fromKey =
    fromIntegral . fromSqlKey

