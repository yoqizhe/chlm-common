{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings          #-}

module Model.Decimal where

import Database.Persist.Sql (PersistField(..), PersistFieldSql(..), SqlType(..), PersistValue(..))
import Data.Decimal (Decimal, realFracToDecimal)
import Data.Aeson (FromJSON(..), ToJSON(..), withScientific, Value(Number))
import Data.Text (Text, pack, unpack)
import Text.Read (readMaybe)
import qualified Data.Text as T

instance PersistField Decimal where
  toPersistValue decimal = PersistText $ T.pack $ show decimal -- 将 Decimal 转换为字符串存储
  fromPersistValue (PersistDouble d) =
    let decimal = realFracToDecimal 2 (realToFrac d)
    in Right decimal
  fromPersistValue _ = Left "Decimal must be converted from fucK"

instance PersistFieldSql Decimal where
    sqlType _ = SqlString

instance FromJSON Decimal where
  parseJSON = withScientific "Decimal" $ \n ->
    return (realToFrac n)

instance ToJSON Decimal where
  toJSON decimal = Number (realToFrac decimal)

