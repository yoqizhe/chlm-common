{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Support.Service
( ServiceContext(..)
, RawUserIdGetter(..)
, ServeFor
, ServiceException(..)
, RunService(..)
, MonadServe(..)
, ServiceContextReader(..)
, ctxTime
, ctxUserId
, ctxData
, runServeFor
, tryRunService
, failS
, failWhenS
, checkDomainExistS
) where

import Data.Time.LocalTime (LocalTime)
import Control.Monad.Trans.Reader (ReaderT(..), ask, runReaderT)
import Control.Monad.Trans.Class (lift)
import Yesod.Core.Types (HandlerFor)
import ClassyPrelude.Yesod (MonadHandler(..), MonadResource(..))
import Control.Monad.Logger (MonadLogger(..))
import Support.Response(ExceptionResponse(..), responseFail, responseBusinessFail)
import Data.Text (Text, pack)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad (when)
import UnliftIO.Exception(Exception(..), SomeException, catch, throwIO)
import UnliftIO (MonadUnliftIO(..))

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

data ServiceContext site a = ServiceContext
    { serviceCtxUserId :: RawUserId site
    , serviceCtxTime :: LocalTime
    , serviceData :: a
    }

-- | Type alias for service handlers that can throw exceptions.
newtype ServeFor site param a = ServeFor (ReaderT (ServiceContext site param) (HandlerFor site) a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadResource, MonadLogger)

instance MonadUnliftIO (ServeFor site param) where
    withRunInIO :: ((forall a. ServeFor site param a -> IO a) -> IO b) -> ServeFor site param b
    withRunInIO inner = ServeFor $ withRunInIO $ \runInIO ->
        inner (runInIO . runDbServeFor)
      where
        runDbServeFor (ServeFor r) = r

instance MonadHandler (ServeFor site param) where
    type HandlerSite (ServeFor site param) = site
    liftHandler handler = ServeFor $ lift handler

class Monad m => ServiceContextReader m where
    type ScSite m
    type ScParam m
    ctx :: m (ServiceContext (ScSite m) (ScParam m))

instance ServiceContextReader (ServeFor site param) where
    type ScSite (ServeFor site param) = site
    type ScParam (ServeFor site param) = param
    ctx = ServeFor ask

ctxTime :: (ServiceContextReader m) => m LocalTime
ctxTime = serviceCtxTime <$> ctx

ctxUserId :: (ServiceContextReader m) => m (RawUserId (ScSite m))
ctxUserId = serviceCtxUserId <$> ctx

ctxData :: (ServiceContextReader m) => m  (ScParam m)
ctxData = serviceData <$> ctx

class Monad m => MonadServe m where
    type ServeSite m
    type ServeParam m
    liftS :: ServeFor (ServeSite m) (ServeParam m) a -> m a
    liftSwP :: ServeFor (ServeSite m) param a -> param -> m a
    

instance MonadServe (ServeFor site param) where
    type ServeSite (ServeFor site param) = site
    type ServeParam (ServeFor site param) = param
    liftS = id
    liftSwP service param = do
        ServiceContext userId now _ <- ctx
        let newCtx = ServiceContext userId now param
        liftHandler $ runServeFor service newCtx
    

-- 实现 MonadResource 实例
-- instance MonadResource (ServeFor site param) where
--     liftResourceT = ServeFor . lift . liftResourceT

-- 实现 MonadLogger 实例
-- instance MonadLogger (ServeFor site param) where
--     -- 这里只是一个示例实现，可以根据实际需求调整
--     monadLoggerLog loc src lvl msg = ServeFor $ lift $ monadLoggerLog loc src lvl msg

runServeFor :: ServeFor site param a -> ServiceContext site param -> HandlerFor site a
runServeFor (ServeFor service) context = runReaderT service context

class RunService site where

    -- runService :: ServeFor site param result -> param -> HandlerFor site result
    runService :: ServeFor site param result -> param -> HandlerFor site result

tryRunService :: (RunService site) => ServeFor site param result -> param -> HandlerFor site (Either ServiceException result)
tryRunService service param =
    (Right <$> runService service param) `catch` handleServiceException
    where
        handleServiceException :: SomeException -> HandlerFor site (Either ServiceException a)
        handleServiceException e = 
            case fromException e of
                Just (ex :: ServiceException) -> return $ Left ex
                Nothing -> throwIO $ UnexpectedExcpetion $ pack $ show e

failS :: MonadIO m => Text -> m ()
failS = throwIO . GeneralServiceException

failWhenS :: MonadIO m => Text -> Bool -> m ()
failWhenS tip condition = when condition (failS tip)

checkDomainExistS :: MonadIO m => Text -> Maybe a -> m a
checkDomainExistS tip target = do
    case target of
        Nothing -> throwIO $ DomainNotFoundException tip
        Just o -> return o