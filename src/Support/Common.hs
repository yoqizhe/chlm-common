{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Support.Common where

import qualified Data.Aeson as J
import Data.Aeson                 (Result (..), fromJSON, withObject, decode, eitherDecode, (.!=),
                                   (.:?))
import qualified Network.HTTP.Types            as H
import qualified Data.Text as T
import Data.Text
import Data.ByteString (ByteString)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Time.Calendar (Day, fromGregorian)
import Data.Maybe (fromMaybe, catMaybes, mapMaybe)
import Text.Read (readMaybe)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Text.Lazy as TL
import UnliftIO.Exception
import Control.Monad.Trans.Reader (ReaderT(..), ask, runReaderT)
import Data.List (groupBy, sortBy)
import Data.Function (on)
import qualified Data.List as List
import Yesod.Core
import Support.Response
import Support.DateTime
import Data.Time (LocalTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)

requireJsonFromRequest :: (MonadHandler m, J.FromJSON a) => m a
requireJsonFromRequest = do
    ra <- parseCheckJsonBody
    case ra of
        J.Error s -> responseParamFail (T.pack s)
        J.Success a -> return a

requirePageParams :: MonadHandler m => m (Int, Int)
requirePageParams = do
    pageNumText <- lookupGetParam "page_num"
    pageSizeText <- lookupGetParam "page_size"
    let pageNum = fromMaybe 1 (pageNumText >>= readMaybe . T.unpack)
    let pageSize = fromMaybe 10 (pageSizeText >>= readMaybe . T.unpack)
    return (pageNum, pageSize)

textToInt :: Text -> Maybe Int
textToInt = readMaybe . T.unpack

textsToInts :: [Text] -> [Int]
textsToInts = mapMaybe textToInt

lookupMaybeIntParam :: MonadHandler m => Text -> m (Maybe Int)
lookupMaybeIntParam = ((>>= textToInt) <$> ) . lookupGetParam

lookupIntParam :: MonadHandler m => Text -> m Int
lookupIntParam param = do 
    mResult <- lookupMaybeIntParam param
    case mResult of
        Nothing -> sendStatusJSON H.status400 $ toJSON $ (RError 400 param :: R T.Text)
        Just a -> return a

lookupMaybeLocalTimeParam :: MonadHandler m => Text -> m (Maybe LocalTime)
lookupMaybeLocalTimeParam param = do
    mResult <- lookupGetParam param
    mapM parseLocalTime mResult
    where
        parseLocalTime :: MonadHandler m => Text -> m LocalTime
        parseLocalTime dateStr = do
            case parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" (unpack dateStr) of
                Just utcTime -> do
                    localTime <- liftIO $ convertToLocalTime utcTime
                    return localTime
                Nothing -> sendStatusJSON H.status400 $ toJSON $ (RError 400 param :: R T.Text)

lookupArrayParam :: MonadHandler m => Text -> Text -> m [Text]
lookupArrayParam param splitFlag = do 
    mResult <- lookupGetParam param
    case mResult of
        Nothing -> return []
        Just t -> return $ T.splitOn splitFlag t

lookUpArrayIntParam :: MonadHandler m => Text -> Text -> m [Int]
lookUpArrayIntParam = ((textsToInts <$>) <$>) . lookupArrayParam

jsontext2data :: FromJSON a => Text -> Either Text a
jsontext2data jsonText = 
    let jsonBytes = TLE.encodeUtf8 $ TL.fromStrict jsonText  -- 将 Text 转为 ByteString
    in case eitherDecode jsonBytes of
        Left err -> Left (T.pack err)  -- 将 String 错误信息转换为 Text
        Right val -> Right val

hoistReaderT :: (m a -> n a) -> ReaderT r m a -> ReaderT r n a
hoistReaderT nat (ReaderT f) = ReaderT (nat . f)

groupByField :: Ord b => (a -> b) -> [a] -> [(b, [a])]
groupByField field items = 
    let tuples = (\items -> (field (List.head items), items)) <$> groupByFieldList field items
    in sortBy (compare `on` fst) tuples
    where
        groupByFieldList :: Ord b => (a -> b) -> [a] -> [[a]]
        groupByFieldList field = List.groupBy ((==) `on` field) . sortBy (compare `on` field)