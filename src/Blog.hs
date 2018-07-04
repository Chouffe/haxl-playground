{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Blog where

-- TODO: better imports and namespacing
import           Control.Exception       (bracket)
import           Control.Monad.Logger    (LoggingT, runStdoutLoggingT)
import           Data.Int
import qualified Data.Pool               as Pool
import           Data.Text               (Text)
import           Database.Persist.Sql
import           Database.Persist.Sqlite

import           Model

data Config
  = Config
    { cConnectionString  :: SqliteInfo
    , cNumberConnections :: Int
    }
  deriving (Show, Eq)

defaultConfig :: Config
defaultConfig = Config "db.sqlite" 1

-- TODO: make sqlite work with a pool of connections
data Handle
  = Handle
    { hConfig :: Config
    , hPool   :: Pool.Pool SqlBackend
    }


mkSqlitePool :: IO (Pool.Pool SqlBackend)
mkSqlitePool = do
  pool <- runStdoutLoggingT $ createSqlitePool "db.sqlite" 5
  return pool

-- TODO: clean up pool and use bracket instead here
withHandle :: Config -> (Handle -> IO a) -> IO a
withHandle config f = do
  pool <- mkSqlitePool
  f $ Handle config pool

type Blog a = SqlPersistT (LoggingT IO) a
type PostInfoKey = Int64
type SqliteInfo = Text

getPostIds :: Blog [PostInfoKey]
getPostIds = do
  entities <- selectList [PostInfoId >. (toSqlKey 0)] []
  return $ (fromSqlKey . entityKey) <$> entities

getPostContent :: PostInfoKey -> Blog (Maybe PostContent)
getPostContent k = get (toSqlKey k)

multiGetPostContents :: [PostInfoKey] -> Blog [PostContent]
multiGetPostContents ids = do
  entities <- selectList [PostContentPostId <-. (toSqlKey <$> ids)] []
  return $ entityVal <$> entities

run :: Config -> Blog a -> IO a
run conf query =
  runStdoutLoggingT
    $ withSqliteConn (cConnectionString conf)
    $ \backend -> runSqlConn query backend

run' :: Handle -> Blog a -> IO a
run' h query = runStdoutLoggingT $ runSqlPool query (hPool h)

action' :: IO [Maybe PostContent]
action' = withHandle defaultConfig
  $ (\h -> run' h $ do
      ids <- getPostIds
      mapM getPostContent ids)

-- triggers N+1 queries
action :: IO [Maybe PostContent]
action = run defaultConfig $ do
  ids <- getPostIds
  mapM getPostContent ids
-- [Debug#SQL] SELECT "id", "date", "topic" FROM "postinfo" WHERE ("id">?); [PersistInt64 0]
-- [Debug#SQL] SELECT "id", "content", "post_id" FROM "postcontent" WHERE "id"=? ; [PersistInt64 1]
-- [Debug#SQL] SELECT "id", "content", "post_id" FROM "postcontent" WHERE "id"=? ; [PersistInt64 2]
-- [Debug#SQL] SELECT "id", "content", "post_id" FROM "postcontent" WHERE "id"=? ; [PersistInt64 3]
-- [Debug#SQL] SELECT "id", "content", "post_id" FROM "postcontent" WHERE "id"=? ; [PersistInt64 4]
-- [Debug#SQL] SELECT "id", "content", "post_id" FROM "postcontent" WHERE "id"=? ; [PersistInt64 5]
-- [Debug#SQL] SELECT "id", "content", "post_id" FROM "postcontent" WHERE "id"=? ; [PersistInt64 6]
-- [Debug#SQL] SELECT "id", "content", "post_id" FROM "postcontent" WHERE "id"=? ; [PersistInt64 7]
-- [Debug#SQL] SELECT "id", "content", "post_id" FROM "postcontent" WHERE "id"=? ; [PersistInt64 8]
-- [Debug#SQL] SELECT "id", "content", "post_id" FROM "postcontent" WHERE "id"=? ; [PersistInt64 9]
-- [Debug#SQL] SELECT "id", "content", "post_id" FROM "postcontent" WHERE "id"=? ; [PersistInt64 10]
-- [Debug#SQL] SELECT "id", "content", "post_id" FROM "postcontent" WHERE "id"=? ; [PersistInt64 11]
-- [Debug#SQL] SELECT "id", "content", "post_id" FROM "postcontent" WHERE "id"=? ; [PersistInt64 12]

action2 :: IO [PostContent]
action2 = run defaultConfig $ do
  ids <- getPostIds
  multiGetPostContents ids
-- Only 2 queries
-- [Debug#SQL] SELECT "id", "date", "topic" FROM "postinfo" WHERE ("id">?); [PersistInt64 0]
-- [Debug#SQL] SELECT "id", "content", "post_id" FROM "postcontent" WHERE ("post_id" IN (?,?,?,?,?,?,?,?,?,?,?,?)); [PersistInt64 1,PersistInt64 2,PersistInt64 3,PersistInt64 4,PersistInt64 5,PersistInt64 6,PersistInt64 7,PersistInt64 8,PersistInt64 9,PersistInt64 10,PersistInt64 11,PersistInt64 12]


-- We would like to automate the batching
