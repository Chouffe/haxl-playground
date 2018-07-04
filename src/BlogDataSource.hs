{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

module BlogDataSource where

import           Control.Monad
import           Data.Hashable   (Hashable, hashWithSalt)
import qualified Data.Map        as Map
import           Data.Text       (Text)
import           Haxl.Core
import           Type.Reflection (Typeable)

import           Blog            (Config (..), Handle (..), run', withHandle)
import qualified Blog
import           Model

-- Types

type PostId =  Int

-- Request Type

data BlogRequest a where
  FetchPosts       :: BlogRequest [PostId]
  FetchPostContent :: PostId -> BlogRequest PostContent
  deriving (Typeable)

deriving instance Eq (BlogRequest a)
deriving instance Show (BlogRequest a)
instance ShowP BlogRequest where showp = show

instance Hashable (BlogRequest a) where
  hashWithSalt s FetchPosts            = hashWithSalt s (0 :: Int)
  hashWithSalt s (FetchPostContent id) = hashWithSalt s (1 :: Int, id)

-- Requests

getPostIds :: GenHaxl u [PostId]
getPostIds = dataFetch FetchPosts

getPostContent :: PostId -> GenHaxl u PostContent
getPostContent = dataFetch . FetchPostContent

-- DataSource Implementation

instance DataSourceName BlogRequest where
  dataSourceName _ = "BlogDataSource"

initDataSource :: Handle -> IO (State BlogRequest)
initDataSource h  = return $ BlogDataState h

instance StateKey BlogRequest where
  data State BlogRequest = BlogDataState Handle

instance DataSource u BlogRequest where
  fetch (BlogDataState h) _flags _userEnv blockedFetches = SyncFetch $ do

    unless (null allIdVars) $ do
      allIds <- (run' h Blog.getPostIds) :: IO [Blog.PostInfoKey]
      mapM_ (\r -> putSuccess r (fromIntegral <$> allIds)) allIdVars

    unless (null ids) $ do
      postContents <- run' h (Blog.multiGetPostContents (fromIntegral <$> ids))
      mapM_ (uncurry putSuccess) (zip vars postContents)

    where
      -- Group blockedFetches by types
      allIdVars :: [ResultVar [PostId]]
      allIdVars = [r | BlockedFetch FetchPosts r  <- blockedFetches]

      ids :: [PostId]
      vars :: [ResultVar Model.PostContent]
      (ids, vars) = unzip [(userId, r) | BlockedFetch (FetchPostContent userId) r <- blockedFetches]
