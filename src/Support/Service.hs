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

module Support.Service where

import Control.Exception (Exception)
import Data.Time.LocalTime (LocalTime)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Database.Persist.Sql (SqlBackend, runSqlPool, rawExecute, PersistValue, RawSql, rawSql, insert, fromSqlKey, replace, getBy)
import Database.Persist
import Control.Monad.Trans.Class (lift)
import Yesod.Core.Types (HandlerFor, HandlerData)
import ClassyPrelude.Yesod (YesodDB, runDB, YesodPersist, catch, YesodPersistBackend)
import Support.Db
import Support.DateTime
import Support.Response
import Data.Text (Text, pack)
import Control.Monad.IO.Class (liftIO)
import qualified Network.HTTP.Types            as H
import UnliftIO.Exception

data ServiceException
    = DomainNotFoundException Text  
    | DomainExistException Text      
    | GeneralServiceException Text    
    | UnexpectedExcpetion Text  
    deriving (Show)

instance Exception ServiceException

instance ExceptionResponse ServiceException where
    responseException e = case e of
                DomainExistException tip -> responseBusinessFail $  tip <> "已经存在" 
                DomainNotFoundException tip -> responseBusinessFail $  tip <> "不存在"
                GeneralServiceException tip -> responseBusinessFail tip
                UnexpectedExcpetion tip -> responseFail tip
                _ -> responseFail $ pack $ show e

class RawUserIdGetter site where
    type RawUserId site :: *
    getRawUserId :: HandlerFor site (RawUserId site)

data RawUserIdGetter site => ServiceContext site = ServiceContext
    { serviceCtxUserId :: RawUserId site
    , serviceCtxTime :: LocalTime
  -- , dbConnection
    }

-- Define the Domain type class
class (RawUserIdGetter site, PersistEntity (Po domain), PersistEntityBackend (Po domain) ~ SqlBackend) => PoConverter site domain where
    type Po domain
    domainToInsertPo :: domain -> RawUserId site -> LocalTime -> Po domain
    domainToUpdatePo :: Po domain -> domain -> RawUserId site -> LocalTime -> Po domain

class (PersistEntity po, PersistEntityBackend po ~ SqlBackend) => DomainConverter po where
    type Domain po
    poToDomain :: po -> Domain po

-- | Type alias for service handlers that can throw exceptions.
type ServeFor site a = (RawUserIdGetter site) => ReaderT (ServiceContext site) (HandlerFor site) a

class Service service site where
    type Output service
    action :: service -> ServeFor site (Output service)

class ServiceRunner site where

    runService :: (Service service site) => service -> HandlerFor site (Output service)

tryRunService :: (ServiceRunner site, Service service site) => service -> HandlerFor site (Either ServiceException (Output service))
tryRunService service =
    (runService service >>= return . Right) `catch` handleServiceException

handleServiceException :: SomeException -> HandlerFor site (Either ServiceException a)
handleServiceException e = 
    case fromException e of
        Just (ex :: ServiceException) -> return $ Left ex
        Nothing -> throwIO $ UnexpectedExcpetion $ pack $ show e

toInsertPo :: forall site domain m. (RawUserIdGetter site, PoConverter site domain, Monad m) 
           => domain -> ReaderT (ServiceContext site) m (Po domain)
toInsertPo domain = do
    ctx <- ask
    return $ domainToInsertPo @site domain (serviceCtxUserId ctx) (serviceCtxTime ctx)

toUpdatePo :: forall site domain m. (RawUserIdGetter site, PoConverter site domain, Monad m) 
           => Po domain -> domain -> ReaderT (ServiceContext site) m (Po domain)
toUpdatePo po domain = do
    ctx <- ask
    return $ domainToUpdatePo @site po domain (serviceCtxUserId ctx) (serviceCtxTime ctx)

insertDo :: forall site domain m id. (RawUserIdGetter site, PoConverter site domain, YesodPersist site, YesodPersistBackend site ~ SqlBackend, SafeToInsert (Po domain)) 
           => domain -> ReaderT (ServiceContext site) (HandlerFor site) (Key (Po domain))
insertDo domain = do
    ctx <- ask
    po <- runReaderT (toInsertPo @site domain) ctx
    result <- lift $ tryRunDB $ insert po
    let serviceResult = dbException2serviceException result
    case serviceResult of
        Left e -> throwIO e 
        Right r -> return r

updateDo :: forall site domain m id. (RawUserIdGetter site, PoConverter site domain, YesodPersist site, YesodPersistBackend site ~ SqlBackend) 
           => Key (Po domain) -> Po domain -> domain -> ReaderT (ServiceContext site) (HandlerFor site) ()
updateDo poId oldPo domain = do
    ctx <- ask
    po <- runReaderT (toUpdatePo @site oldPo domain) ctx
    liftIO $ print poId
    result <- lift $ tryRunDB $ replace poId po
    let serviceResult = dbException2serviceException result
    case serviceResult of
        Left e -> throwIO e 
        Right r -> return r

runOnExistPo :: forall site record m e a. 
              (YesodPersist site
              , YesodPersistBackend site ~ SqlBackend 
              , PersistEntity record
              , PersistEntityBackend record ~ SqlBackend
              ) 
           => Key record
           -> Text 
           -> (record -> ReaderT (ServiceContext site) (HandlerFor site) a) 
           -> ReaderT (ServiceContext site) (HandlerFor site) a
runOnExistPo poId tip action = do
    mPo <- lift $ runDB $ get poId
    case mPo of
        Nothing -> throwIO $ DomainNotFoundException tip
        Just po -> action po

runOnUniquePo :: forall site record m a. 
              (YesodPersist site
              , YesodPersistBackend site ~ SqlBackend 
              , PersistEntity record
              , PersistEntityBackend record ~ SqlBackend
              ) 
           => Unique record
           -> Text 
           -> ReaderT (ServiceContext site) (HandlerFor site) a
           -> ReaderT (ServiceContext site) (HandlerFor site) a
runOnUniquePo uniq tip action = do
    mPo <- lift $ runDB $ getBy uniq
    case mPo of
        Just _ -> throwIO $ DomainExistException tip
        Nothing -> action


runOnExceptMeUniquePo :: forall site record m a. 
              (YesodPersist site
              , YesodPersistBackend site ~ SqlBackend 
              , PersistEntity record
              , PersistEntityBackend record ~ SqlBackend
              ) 
           => Key record
           -> Unique record
           -> Text 
           -> ReaderT (ServiceContext site) (HandlerFor site) a
           -> ReaderT (ServiceContext site) (HandlerFor site) a
runOnExceptMeUniquePo key uniq tip action = do
    mPo <- lift $ runDB $ getBy uniq
    case mPo of
        Just (Entity foundKey _)
            | foundKey == key -> action
            | otherwise -> throwIO $ DomainExistException tip
        Nothing -> action

dbException2serviceException :: Either DatabaseException a -> Either ServiceException a
dbException2serviceException (Left (GeneralDatabaseException msg)) = Left $ GeneralServiceException msg
dbException2serviceException (Left DuplicateEntryException) = Left $ GeneralServiceException "重复异常"
dbException2serviceException (Right val) = Right val

