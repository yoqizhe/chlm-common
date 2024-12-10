{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

module Support.DbService
( DbServeFor
, PoConverter(..)
, DomainConverter(..)
, runDBS
, dbsAction
, tryDbsAction
, liftDSwP
, insertDo
, insertDos
, updateDo
, checkDoUnique
, checkDoUniqueExceptMe
, getExistEntity
, getExistKey
, getExistPo
, checkExist
)
where

import Control.Exception (Exception)
import Data.Time.LocalTime (LocalTime)
import Control.Monad.Trans.Reader (ReaderT(..), ask, runReaderT)
import Database.Persist.Sql (SqlBackend, runSqlPool, rawExecute, PersistValue, RawSql, rawSql, insert, fromSqlKey, replace, getBy)
import Database.Persist
import Control.Monad.Trans.Class (lift)
import Yesod.Core.Types (HandlerFor (HandlerFor), HandlerData)
import ClassyPrelude.Yesod (YesodDB, runDB, YesodPersist, catch, YesodPersistBackend, MonadHandler(..), MonadResource(..))
import Control.Monad.Logger (MonadLogger(..), logDebug, logInfo, logWarn, logError)
import Support.Common
import Support.Db
import Support.DateTime
import Support.Service

import Data.Text (Text, pack)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (when, forM, forM_)
import UnliftIO.Exception
import UnliftIO (MonadUnliftIO(..))
import ClassyPrelude (void)
import Data.Conduit.Process.Typed (checkExitCode)

-- 为什么不返回m而是返回ServeFor，因为实现ServiceContextReader需要
newtype DbServeFor site param a = DbServeFor (ReaderT (YesodPersistBackend site) (ServeFor site param) a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadResource, MonadLogger) -- , MonadServe site param

instance MonadUnliftIO (DbServeFor site param) where
    withRunInIO inner = DbServeFor $ withRunInIO $ \runInIO ->
        inner (runInIO . runDbServeFor)
      where
        runDbServeFor (DbServeFor r) = r

instance MonadServe (DbServeFor site param) where
    type ServeSite (DbServeFor site param) = site
    type ServeParam (DbServeFor site param) = param
    liftS = DbServeFor . lift
    liftSwP service param = do
        ServiceContext userId now _ <- liftS ctx
        let newCtx = ServiceContext userId now param
        liftHandler $ runServeFor service newCtx

instance ServiceContextReader (DbServeFor site param) where
    type ScSite (DbServeFor site param) = site
    type ScParam (DbServeFor site param) = param
    
    ctx = liftS ctx

-- 将ReaderT conn (ServeFor site param) a 替换为 ReaderT conn (ServeFor site param2) a
liftDSwP :: forall site param2 result param. DbServeFor site param2 result -> param2 -> DbServeFor site param result
liftDSwP dbService param = callInnerService dbService param
    where
        callInnerService :: DbServeFor site param2 a -> param2 -> DbServeFor site param a
        callInnerService (DbServeFor dbService) param = DbServeFor $ hoistReaderT (flip liftSwP param) dbService

instance MonadHandler (DbServeFor site param) where
    type HandlerSite (DbServeFor site param) = site
    -- liftHandler handler = DbServeFor . hoistReaderT ServeFor $ lift $ lift handler
    liftHandler :: HandlerFor (HandlerSite (DbServeFor site param)) a -> DbServeFor site param a
    liftHandler = liftS . liftHandler

dbsAction :: YesodDB site a -> DbServeFor site param a
dbsAction action = DbServeFor $ hoistReaderT liftHandler action

tryDbsAction :: DbServeFor site param a -> DbServeFor site param (Either DatabaseException a)
tryDbsAction action = (Right <$> action) `catch` handleDBException

runDBS :: (YesodPersist site, RunService site) => DbServeFor site param a -> ServeFor site param a
runDBS dbService = ctx >>= (liftHandler . runDB . runInnerService dbService)
        where
            -- 不能用runService，runService会重新生成上下文
            runInnerService :: DbServeFor site param a -> ServiceContext site param -> YesodDB site a
            runInnerService (DbServeFor dbService) context = hoistReaderT (flip runServeFor context) dbService

-- db 相关
class (RawUserIdGetter site, PersistEntity (Po domain), PersistEntityBackend (Po domain) ~ SqlBackend) => PoConverter site domain where
    type Po domain
    domainToInsertPo :: domain -> RawUserId site -> LocalTime -> Po domain
    domainToUpdatePo :: Po domain -> domain -> RawUserId site -> LocalTime -> Po domain

class (PersistEntity po, PersistEntityBackend po ~ SqlBackend) => DomainConverter po where
    type Domain po
    poToDomain :: po -> Domain po

toInsertPo :: forall site param domain. (RawUserIdGetter site, PoConverter site domain)
           => domain -> DbServeFor site param (Po domain)
toInsertPo domain = do
    ServiceContext userId now _ <- ctx
    return $ domainToInsertPo @site domain userId now

toUpdatePo :: forall site domain param. (RawUserIdGetter site, MonadServe (DbServeFor site param), PoConverter site domain)
           => Po domain -> domain -> DbServeFor site param (Po domain)
toUpdatePo po domain = do
    ServiceContext userId now _ <- ctx
    return $ domainToUpdatePo @site po domain userId now

insertDo :: forall site domain param.
            (RawUserIdGetter site, PoConverter site domain
            , YesodPersist site, YesodPersistBackend site ~ SqlBackend, SafeToInsert (Po domain))
           => domain -> DbServeFor site param (Key (Po domain))
insertDo domain = do
    po <- toInsertPo domain
    dbsAction $ insert po

insertDos :: forall site domain param.
            (RawUserIdGetter site, PoConverter site domain
            , YesodPersist site, YesodPersistBackend site ~ SqlBackend, SafeToInsert (Po domain))
           => [domain] -> DbServeFor site param [Key (Po domain)]
insertDos [] = return []
insertDos domains = do
    pos <- mapM toInsertPo domains
    dbsAction $ insertMany pos

updateDo :: forall site domain param. (RawUserIdGetter site, PoConverter site domain, YesodPersist site, YesodPersistBackend site ~ SqlBackend)
           => Entity (Po domain) -> domain -> DbServeFor site param ()
updateDo (Entity poId oldPo) domain = do
    po <- toUpdatePo @site oldPo domain
    dbsAction $ replace poId po

getExistEntity :: forall site record param a.
              (YesodPersist site
              , YesodPersistBackend site ~ SqlBackend
              , PersistEntity record
              , PersistEntityBackend record ~ SqlBackend
              )
           => Text -> Key record -> DbServeFor site param (Entity record)
getExistEntity tip poId = do
    mPo <- dbsAction $ get poId
    case mPo of
        Nothing -> throwIO $ DomainNotFoundException tip
        Just po -> return (Entity poId po)

getExistKey :: forall site record param.
              (YesodPersist site
              , YesodPersistBackend site ~ SqlBackend
              , PersistEntity record
              , PersistEntityBackend record ~ SqlBackend
              )
           => Text -> Key record -> DbServeFor site param (Key record)
getExistKey tip poId = entityKey <$> getExistEntity tip poId

getExistPo :: forall site record param.
              (YesodPersist site
              , YesodPersistBackend site ~ SqlBackend
              , PersistEntity record
              , PersistEntityBackend record ~ SqlBackend
              )
           => Text -> Key record -> DbServeFor site param record
getExistPo tip poId = entityVal <$> getExistEntity tip poId

checkExist :: forall site record param.
              (YesodPersist site
              , YesodPersistBackend site ~ SqlBackend
              , PersistEntity record
              , PersistEntityBackend record ~ SqlBackend
              )
           => Text -> Key record -> DbServeFor site param ()
checkExist tip poId = void $ getExistEntity tip poId

checkDoUnique :: forall site domain param.
            (RawUserIdGetter site, PoConverter site domain
            , YesodPersist site, YesodPersistBackend site ~ SqlBackend)
           => Text -> domain -> DbServeFor site param domain
checkDoUnique tip domain = do
    po <- toInsertPo domain
    mUniq <- dbsAction $ checkUnique po
    case mUniq of
        Just _ -> throwIO $ DomainExistException tip
        Nothing -> return domain

checkDoUniqueExceptMe :: forall site domain param record.
            (RawUserIdGetter site, PoConverter site domain
            , YesodPersist site, YesodPersistBackend site ~ SqlBackend
            , PersistEntity record, PersistEntityBackend record ~ SqlBackend
            , Po domain ~ record)
           => Text -> (Key record, domain) -> DbServeFor site param domain
checkDoUniqueExceptMe tip (recordId, domain) = do
    po <- toInsertPo domain
    let uniqueKeys = persistUniqueKeys po
    forM_ uniqueKeys $ isUniqExceptMe recordId
    return domain
    where
        isUniqExceptMe ::Key record -> Unique record -> DbServeFor site param ()
        isUniqExceptMe key uniqueKey = do
            result <- dbsAction $ getBy uniqueKey
            case result of
                -- 如果找到冲突，检查冲突记录的 ID 是否等于 currentId
                Just (Entity existingId _)
                    | existingId == key -> return ()
                    | otherwise -> throwIO $ DomainExistException tip
                Nothing -> return ()