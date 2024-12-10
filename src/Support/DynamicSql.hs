{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Support.DynamicSql 
( SqlVal(..)
, DynamicSql(..)
, parseDynamicSql
)
where

import Data.Text (Text, intercalate)
import qualified Data.Text as T
import Database.Persist.Sql (SqlBackend, runSqlPool, rawExecute, PersistValue, RawSql, rawSql, toPersistValue, PersistField, fromSqlKey, ToBackendKey)
import Database.Persist
import Data.Maybe (mapMaybe, catMaybes, fromMaybe)
import Model.Decimal
-- | SqlVal

newtype SqlVal = SqlVal {
    unSqlVal :: (Text, [PersistValue])
}

instance Semigroup SqlVal where
  SqlVal (sql1, val1) <> SqlVal (sql2, val2) = SqlVal (sql1 <> " " <> sql2, val1 ++ val2)

sqlFromnSqlVal :: SqlVal -> Text
sqlFromnSqlVal (SqlVal (sql, _)) = sql

valFromSqlVal :: SqlVal -> [PersistValue]
valFromSqlVal (SqlVal (_, values)) = values

emptySqlVal :: SqlVal
emptySqlVal = SqlVal ("", [])

withSql :: Text -> SqlVal
withSql sqlText = SqlVal (sqlText, [])

withVal :: PersistField a => a -> SqlVal
withVal value = SqlVal ("", [toPersistValue value])

withSqlVal :: PersistField a => Text -> a -> SqlVal
withSqlVal sqlText value = SqlVal (sqlText, [toPersistValue value])

intercalateSqlVal :: Text -> [SqlVal] -> SqlVal
intercalateSqlVal separator sqlVals = SqlVal (intercalate separator $ sqlFromnSqlVal <$> sqlVals, concatMap valFromSqlVal sqlVals)

removeAndOr :: SqlVal -> SqlVal
removeAndOr (SqlVal (txt, vals)) = case T.stripPrefix "and" txt of
                    Just rest -> SqlVal (rest, vals)
                    Nothing   -> case T.stripPrefix "or" txt of
                                    Just rest -> SqlVal (rest, vals)
                                    Nothing   -> SqlVal (txt, vals)

concatSqlVals :: [SqlVal] -> SqlVal
concatSqlVals [] = emptySqlVal
concatSqlVals sqlVals = foldr (<>) emptySqlVal sqlVals

-- | DynamicSql

data DynamicSql where
    ConstSql    :: Text -> DynamicSql
    ValSql      :: PersistField a => a -> DynamicSql
    ConstValSql :: PersistField a => Text -> a -> DynamicSql
    MaybeSql    :: (a -> DynamicSql) -> Maybe a -> DynamicSql
    ForSql      :: Maybe Text -> Maybe Text -> Maybe Text -> [DynamicSql] -> DynamicSql
    WhereSql    :: DynamicSql -> DynamicSql
    PageSql     :: Int -> Int -> DynamicSql
    DynamicSql  :: [DynamicSql] -> DynamicSql

instance Semigroup DynamicSql where
  x <> y = DynamicSql [x, y]

parseDynamicSql :: DynamicSql -> SqlVal
parseDynamicSql (ConstSql text) = withSql text
parseDynamicSql (ValSql value) = withSqlVal "?" value
parseDynamicSql (ConstValSql sql value) = parseDynamicSql $ ConstSql sql <> ValSql value
parseDynamicSql (MaybeSql dynamicSql' mVal) = maybe emptySqlVal (parseDynamicSql . dynamicSql') mVal
parseDynamicSql (ForSql mPrefix mSuffix mSeparator dynamicSqls) =
    let prefix = maybe emptySqlVal withSql mPrefix
        suffix = maybe emptySqlVal withSql mSuffix
        sqlVals = parseDynamicSql <$> dynamicSqls
        separator = fromMaybe "" mSeparator
    in (prefix <> intercalateSqlVal separator sqlVals <> suffix)
parseDynamicSql (WhereSql dynamicSql) =
    let sqlVal = parseDynamicSql dynamicSql
    in withSql "WHERE" <> removeAndOr sqlVal
parseDynamicSql (PageSql page size)
    | page > 0 && size > 0 = parseDynamicSql $ ConstValSql "LIMIT" size <> ConstValSql "OFFSET" ((page - 1) * size)
    | otherwise = emptySqlVal
parseDynamicSql (DynamicSql sqls) = concatSqlVals $ parseDynamicSql <$> sqls