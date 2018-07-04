{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Model where

import           Data.Text               (Text)
import           Data.Time.Clock         (UTCTime (..))
import           Database.Persist.Sql
import           Database.Persist.Sqlite
import qualified Database.Persist.TH     as PTH


PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"]
  [PTH.persistLowerCase|
    PostInfo sql=postinfo
      date UTCTime
      topic Text
      deriving Show Read Eq

    PostContent sql=postcontent
      content Text
      postId PostInfoId
      deriving Show Read Eq

    PostView sql=postviews
      count Int
      postId PostInfoId
      deriving Show Read Eq
  |]

connStringSqlite :: Text
connStringSqlite = "db.sqlite"

migrateSqliteDB :: IO ()
migrateSqliteDB = runSqlite connStringSqlite (runMigration migrateAll)
