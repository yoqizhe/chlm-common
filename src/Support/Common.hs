{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Support.Common where

import qualified Data.Aeson as J
import Data.Aeson                 (Result (..), fromJSON, withObject, decode, eitherDecode, (.!=),
                                   (.:?))
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.Class (lift)
import qualified Network.HTTP.Types            as H
import qualified Data.Text as T
import Data.Text
import qualified Data.Text.Encoding as TE
import Data.Time.Calendar (Day, fromGregorian)
import Data.Maybe (fromMaybe, catMaybes, mapMaybe)
import Text.Read (readMaybe)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.ByteString (ByteString)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import UnliftIO.Exception
import Yesod.Core
import Support.Response

requireJsonFromRequest :: (MonadHandler m, J.FromJSON a) => m a
requireJsonFromRequest = do
    ra <- parseCheckJsonBody
    case ra of
        J.Error s -> sendStatusJSON H.status400 $ toJSON $ (RError 400 (T.pack s) :: R T.Text)
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