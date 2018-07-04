{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module HaxlBlog where

import           Data.IORef
import           Data.Traversable (for)
import           Haxl.Core

import           Blog             (Config (..), Handle (..), defaultConfig,
                                   withHandle)

import           BlogDataSource


type Haxl a = GenHaxl () a

run :: Haxl a -> IO a
run h = withHandle defaultConfig (\handle -> do
    db     <- initDataSource handle
    env    <- initEnv (stateSet db stateEmpty) ()
    result <- runHaxl env h
    stats  <- readIORef $ statsRef env
    displayStats stats
    return result
    )

displayStats :: Stats -> IO ()
displayStats stats = putStrLn $ ppStats stats

haction :: IO ()
haction = do
    putStrLn "Running N+1 query with Haxl"
    (cs, cache) <- run $ do
      ids <- getPostIds
      postContents <- for ids getPostContent
      -- postContents <- mapM getPostContent ids
      c <- dumpCacheAsHaskell
      return (postContents, c)
    putStrLn $ show cs
    putStrLn "End of the Query"
    putStrLn "Cache"
    putStrLn cache
