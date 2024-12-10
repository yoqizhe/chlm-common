{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Support.Response where

import qualified Data.Aeson as J
import Data.Aeson                 (Result (..), fromJSON, withObject, (.!=),
                                   (.:?))
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.Class (lift)
import qualified Network.HTTP.Types            as H
import qualified Data.Text as T
import Control.Exception (Exception)
import Data.Typeable (Typeable, cast)
import Yesod.Core
import Support.Excel


data R a = RSuccess a | RError Int T.Text

instance ToJSON a => ToJSON (R a) where
    toJSON (RSuccess a) = object ["code" .= (200 :: Int), "data" .= a]
    toJSON (RError code msg) = object ["code" .= code, "msg" .= msg, "data" .= Object mempty]

class Exception e => ExceptionResponse e where
    responseException :: MonadHandler m => e -> m J.Value

responseResult :: (MonadHandler m, ExceptionResponse e, ToJSON a, Typeable a) => Either e a -> m J.Value
responseResult result = do
    case result of
        Left e -> responseException e
        Right a -> case cast a of
                   Just () -> responseSuccess
                   Nothing -> responseData a

responseSuccess :: (MonadHandler m) => m J.Value
responseSuccess = do
    returnJson $ toJSON $ RSuccess ("success" :: T.Text)

responseData :: (MonadHandler m, ToJSON a) => a -> m J.Value
responseData content = do
    returnJson $ toJSON $ RSuccess content

responsePage :: (MonadHandler m, ToJSON a) => Int -> a -> m J.Value
responsePage total content = do
    responseData $ object ["data" .= content, "total" .= total] 

responseExcel :: (MonadHandler m, ExcelData a) => T.Text -> a -> m TypedContent
responseExcel excelName excelData = do
    byteData <- convertToExcelBS excelName excelData
    let content = TypedContent "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" (toContent byteData)
    addHeader "Content-Disposition" $ "attachment; filename=" <> "export" <> ".xlsx"
    sendResponseStatus H.status200 content

responseTypeContentError :: (MonadHandler m) => T.Text -> m TypedContent
responseTypeContentError errMsg = do
  let errorJson = object ["error" .= errMsg]
  return $ toTypedContent $ toJSON errorJson

responseFail :: (MonadHandler m) => T.Text -> m a
responseFail msg = do
    sendStatusJSON H.status500 $ toJSON $ (RError 500 msg :: R String)

responseParamFail :: (MonadHandler m) => T.Text -> m a
responseParamFail msg = do
    sendStatusJSON H.status400 $ toJSON $ (RError 400 msg :: R T.Text)

responseBusinessFail :: (MonadHandler m) => T.Text -> m J.Value
responseBusinessFail msg = do
    sendStatusJSON H.status200 $ toJSON $ (RError 500 msg :: R String)