{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Support.Validate where

import qualified Data.Text as T
import Control.Monad.Fail (MonadFail)
import Control.Monad (when)
import Data.Proxy
import Data.Maybe (fromMaybe)
import Data.Time
import Data.Text

extractJust :: MonadFail m => T.Text -> Maybe a -> m a
extractJust label maybeValue = do
    case maybeValue of
        Nothing -> fail $ (T.unpack label) <> "不能为空"
        Just b -> return b

extractNotBlank :: MonadFail m => T.Text -> T.Text -> m T.Text
extractNotBlank label t = do
    if T.null t
        then fail $ T.unpack label <> "不能为空文本"
        else return t

extractPositive :: (MonadFail m, Num a, Ord a) => T.Text -> a -> m a
extractPositive label value = do
    if value <= 0
    then fail $ T.unpack label <> "必须大于0"
    else return value

extractNoNegative :: (MonadFail m, Num a, Ord a) => T.Text -> a -> m a
extractNoNegative label value = do
    if value < 0
    then fail $ T.unpack label <> "不能小于0"
    else return value

extractGt :: (MonadFail m, Num a, Ord a, Show a) => T.Text -> a -> a -> m a
extractGt label limit value = do
    if value <= limit
    then fail $ T.unpack label <> "需大于" <> show limit
    else return value

extractGe :: (MonadFail m, Num a, Ord a, Show a) => T.Text -> a -> a -> m a
extractGe label limit value = do
    if value < limit
    then fail $ T.unpack label <> "需大于等于" <> show limit
    else return value

extractLt :: (MonadFail m, Num a, Ord a, Show a) => T.Text -> a -> a -> m a
extractLt label limit value = do
    if value >= limit
    then fail $ T.unpack label <> "需小于" <> show limit
    else return value

extractLe :: (MonadFail m, Num a, Ord a, Show a) => T.Text -> a -> a -> m a
extractLe label limit value = do
    if value > limit
    then fail $ T.unpack label <> "需小于等于" <> show limit
    else return value

extractDefault :: Monad m => a -> Maybe a -> m a
extractDefault defaultValue maybeValue = return $ fromMaybe defaultValue maybeValue

extractEnum :: forall a m. (Bounded a, Enum a, MonadFail m) => T.Text -> Int -> m a
extractEnum label n = do
    let minVal = fromEnum (minBound :: a)
        maxVal = fromEnum (maxBound :: a)
    when (n < minVal || n > maxVal) $
        fail $ T.unpack label <> " 无效参数： " <> show n
    return (toEnum n)

validateDayRange :: (MonadFail m) => Day -> Day -> m ()
validateDayRange startDay endDay = do
    if startDay >= endDay
    then fail "开始日期大于结束日期"
    else return ()

extractEitherRight :: MonadFail m => (a -> Either Text a) -> a -> m a
extractEitherRight f val = do
    let e = f val
    case e of
        Left msg -> fail $ unpack msg
        Right v -> return v