{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Support.Db where

import Control.Exception (Exception, SomeException)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Text (Text, intercalate)
import qualified Data.Text as T
import Database.Persist.Sql (SqlBackend, runSqlPool, rawExecute, PersistValue, RawSql, rawSql, toPersistValue, PersistField, fromSqlKey, ToBackendKey)
import Database.Persist
import Data.Maybe (mapMaybe, catMaybes, fromMaybe)
import Model.Decimal
import Yesod.Core
-- YesodDB defined in here
import ClassyPrelude.Yesod (YesodDB, runDB, YesodPersist, catch, YesodPersistBackend, catMaybes)
import UnliftIO.Exception

data DatabaseException = GeneralDatabaseException Text | DuplicateEntryException
    deriving (Show)
instance Exception DatabaseException

handleDBException :: Monad m => SomeException -> m (Either DatabaseException a)
handleDBException e =
    let eMsg = T.pack $ show e
        in
        if "Duplicate" `T.isInfixOf` eMsg
            then return $ Left DuplicateEntryException
            else return $ Left $ GeneralDatabaseException eMsg

tryRunDB :: YesodPersist site => YesodDB site a -> HandlerFor site (Either DatabaseException a)
tryRunDB dbAction =
    (do
        result <- runDB dbAction
        return $ Right result
    ) `catch` handleDBException

onDuplicateKey :: MonadIO m => m (Maybe a) -> Either DatabaseException a -> m (Maybe a)
onDuplicateKey action e = case e of
    Left dbException -> case dbException of
        DuplicateEntryException -> action
        GeneralDatabaseException msg -> throwIO $ GeneralDatabaseException msg
    Right r -> return $ Just r

formatResults :: DbResultFormat a => [ResultTuple a] -> [a]
formatResults results = map formatResult results

class DbResultFormat a where
    type ResultTuple a :: *
    formatResult :: ResultTuple a -> a

class SqlTemplate a where
    createTemplate :: a -> Text
    createParams :: a -> [PersistValue]

class RawSql(RawResult a) => TemplateResult a where
    type RawResult a :: *
    type Result a :: *
    formatTemplateResult :: a -> [RawResult a] -> Result a

-- YesodPersist site, ensure runDB
-- YesodPersistBackend site ~ SqlBackend, ensure rawSql
runSqlTemplate :: (YesodPersist site, SqlTemplate a, YesodPersistBackend site ~ SqlBackend, TemplateResult a)
            => a -> HandlerFor site (Result a)
runSqlTemplate param = do
    let sql = createTemplate param
    let params = createParams param
    let myRawSql = rawSql sql params
    rawResults <- runDB myRawSql
    return $ formatTemplateResult param rawResults

rawEntityKey :: ToBackendKey SqlBackend record => Key record -> Int
rawEntityKey = fromIntegral . fromSqlKey

icontains :: EntityField r T.Text -> T.Text -> Filter r
icontains field val = Filter field (FilterValue $ T.concat ["%", val, "%"]) (BackendSpecificFilter " like ")

someQuestionMarkInBracket :: Int -> Text
someQuestionMarkInBracket questionMarkCount =
    let questionMarks = intercalate "," (replicate questionMarkCount "?")
    in "(" <> questionMarks <> ")"

-- a is key
groupTupleArrayByKey :: Eq a => [(a, b, Maybe c)] -> [(a, b, [c])]
groupTupleArrayByKey = foldr combineTupleToArrayByKey []
    where
        findTupleFromArrayByKey [] _ = Nothing
        findTupleFromArrayByKey ((key, other, acc):xs) key'
            | key == key' = Just (key, other, acc)
            | otherwise = findTupleFromArrayByKey xs key'

        deleteTupleFromArrayByKey acc key = filter (\(key', _, _) -> key' /= key) acc

        combineTupleToArrayByKey (key, other, Nothing) acc = case findTupleFromArrayByKey acc key of
            Just _ -> acc -- do nothing if array exist key
            Nothing -> (key, other, []) : acc -- put tuple to array if array not exist key
        combineTupleToArrayByKey (key, other, Just tag) acc = case findTupleFromArrayByKey acc key of
            Just (_, _, existTags) -> (key, other, tag : existTags) : deleteTupleFromArrayByKey acc key
            Nothing -> (key, other, [tag]) : acc

inSql :: Text -> [a] -> Text
inSql _ [] = ""
inSql col vals = col <> " in " <> someQuestionMarkInBracket (length vals)

limitSql :: Int -> Int -> Text
limitSql page size
    | page > 0 && size > 0 = "limit ? offset ? "
    | otherwise = ""

limitParams :: Int -> Int -> [PersistValue]
limitParams page size
    | page > 0 && size > 0 = [toPersistValue size, toPersistValue ((page - 1) * size)]
    | otherwise = []

limitSelectOpt :: Int -> Int -> [SelectOpt record]
limitSelectOpt pageNum pageSize
    | pageSize > 0 = [LimitTo pageSize, OffsetBy $ (pageNum - 1) * pageSize]
    | otherwise = []
