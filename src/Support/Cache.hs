{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Support.Cache where

import Data.Text
import qualified Data.Text as T

import Yesod.Core

data CachePrefixEnum = CacheListTag | CacheListReciteQuestion | CacheReciteStrategy
    deriving (Show, Enum, Eq, Bounded)

-- prefix and key
data CacheKey a = CacheKey
    { prefix :: CachePrefixEnum
    , key :: a
    }

cacheKeyPrefixText :: CachePrefixEnum -> Text
cacheKeyPrefixText CacheListTag = "recite:list.tag:user.id:"
cacheKeyPrefixText CacheListReciteQuestion = "recite:list.recite.question:user.id:"
cacheKeyPrefixText CacheReciteStrategy = "recite:list.recite.strategy:user.id:"

cacheKeyText :: Show a => CacheKey a -> Text
cacheKeyText (CacheKey prefix key) = cacheKeyPrefixText prefix <> T.pack (show key)

keepAliveSecond :: Int
keepAliveSecond = 1800

class Cache site where

    getFromCache :: (FromJSON a) => Text -> HandlerFor site (Maybe a)

    saveToCache :: (ToJSON a) => Text -> a -> Int -> HandlerFor site ()

    refreshCache :: Text -> Int -> HandlerFor site ()

    deleteCache :: Text -> HandlerFor site ()

findFromCacheBeforeDb :: (Cache site, Show k, ToJSON a, FromJSON a) 
                      => CacheKey k -> (k -> HandlerFor site a) -> HandlerFor site a
findFromCacheBeforeDb cacheKey findFromDB = do
    mValFromCache <- getFromCache $ cacheKeyText cacheKey
    case mValFromCache of
        Just val -> do
            refreshCache (cacheKeyText cacheKey) keepAliveSecond
            return val
        Nothing -> do
            dataFromDb <- findFromDB $ key cacheKey
            saveToCache (cacheKeyText cacheKey) dataFromDb keepAliveSecond
            return dataFromDb

deleteCacheByKey :: (Cache site, Show a) => CacheKey a -> HandlerFor site ()
deleteCacheByKey = deleteCache . cacheKeyText

            
   