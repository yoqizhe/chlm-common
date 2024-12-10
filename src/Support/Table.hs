{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

module Support.Table
( Dimension(..)
, table2array
, emptyTable
, RowTable
, createRowTable
, emptyRowTable
, sumRowTable
, addRow
, toColTable
, ColTable
, KeyRowTable
, createKeyRowTable
, joinKeyRowTable
, leftJoinKeyRowTable
)
where

import Yesod.Core
import Data.Decimal (Decimal)
import Data.Time.LocalTime (LocalTime)
import Data.Text (Text, pack)
import qualified Data.ByteString.Lazy as L
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Ord (comparing)
import Model.Decimal
import Data.List (nub, sort, sortBy, (\\), group, transpose)
import Data.Function (on)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad (when, forM, forM_)
import Data.Maybe (mapMaybe, catMaybes, fromMaybe)

class Dimension a where
    type DimensionContent a
    to1d :: a -> [DimensionContent a]
    dimensionLength :: a -> Int

instance Dimension [a] where
    type DimensionContent [a] = a
    to1d array = array
    dimensionLength = length

class Table a where
    type ToArrayContent a
    table2array :: a -> ([Text], [[ToArrayContent a]])
    emptyTable :: a

data Title a = Title Text a

-- why keep a, because title
data RowTable a row = RowTable [Title a] [row]

instance Semigroup row => Semigroup (RowTable a row) where
    RowTable titles1 rows1 <> RowTable titles2 rows2 = RowTable (titles1 ++ titles2) (zipWith (<>) rows1 rows2)

instance (Dimension row, DimensionContent row ~ a) => Table (RowTable a row) where
    type ToArrayContent (RowTable a row) = a
    table2array (RowTable titles rows) = ([ name | Title name _<- titles], to1d <$> rows)
    emptyTable = RowTable [] []

instance Functor (RowTable a) where
    fmap :: (a2 -> b) -> RowTable a1 a2 -> RowTable a1 b
    fmap f (RowTable titles rows) = RowTable titles (map f rows)

data ColTable column = ColTable [column]

instance Semigroup (ColTable column) where
  ColTable cols1 <> ColTable cols2 = ColTable (cols1 ++ cols2)

type TitleColTable a = ColTable (Title a, [a])

newtype KeyRow key a = KeyRow (key, [a])

instance Semigroup (KeyRow key a) where
  KeyRow (key1, vals1) <> KeyRow (_, vals2) = KeyRow (key1, vals1 ++ vals2)

instance Dimension (KeyRow key a) where
    type DimensionContent (KeyRow key a) = a
    to1d (KeyRow tuple) = snd tuple
    dimensionLength (KeyRow tuple) = length (snd tuple)

type KeyRowTable key a = RowTable a (KeyRow key a)

-- | Row Table Outer Fun
createRowTable :: (Dimension row, DimensionContent row ~ a) => [(Text, a)] -> [row] -> Either Text (RowTable a row)
createRowTable [] rows = Left "table title is empty"
createRowTable titles rows
    | any ((/= length titles) . dimensionLength) rows = Left "Not all rows have the same number of columns as titles."
    | otherwise = Right $ RowTable (map (uncurry Title) titles) rows

emptyRowTable :: RowTable a row
emptyRowTable = RowTable [] []

toColTable :: (Dimension row, DimensionContent row ~ a) => RowTable a row -> TitleColTable a
toColTable (RowTable titles []) = ColTable $ [ (title, []) | title <- titles]
toColTable (RowTable titles rows) =
    let cols = transpose $ to1d <$> rows
    in ColTable $ zip titles cols

sumRowTable :: (Dimension row, DimensionContent row ~ a, Num a) => (Text -> Bool) -> RowTable a row -> [a]
sumRowTable isMatch rowTable = 
    let colTable = toColTable rowTable
    in sumColTable isMatch colTable

addRow :: (Dimension row, DimensionContent row ~ a) => RowTable a row -> row -> Either Text (RowTable a row)
addRow (RowTable titles rows) newRow
    | dimensionLength newRow == length titles = Right $ RowTable titles (rows <> [newRow])
    | otherwise = Left "add row fail: The length of records does not match the length of titles"

-- | Column Table Outer Fun

sumColTable :: Num a => (Text -> Bool) -> TitleColTable a -> [a]
sumColTable isMatch (ColTable cols) = 
    let targetIdx = [i | (i, name) <- zip [1..] (colTableTitleNames cols), isMatch name]
        sumContents = [ if i `elem` targetIdx then sum column else emptyVal | (i, ((Title _ emptyVal), column)) <- zip [1..] cols]
    in sumContents
    where
        colTableTitleNames :: [(Title a, vals)] -> [Text]
        colTableTitleNames cols = [ title | ((Title title _), _) <- cols]

-- | KeyRow Table Outer Fun
createKeyRowTable :: (Ord key, Show key) 
                    => [(Text, a)] -> [(key, [a])] -> Either Text (KeyRowTable key a)
createKeyRowTable titles rawKeyRows =
    let keys = fst <$> rawKeyRows
        duplicates = [head groupKeys | groupKeys <- group (sort keys), length groupKeys > 1]
        eRowTable = createRowTable titles (KeyRow <$> rawKeyRows)
    in if not (null duplicates)
        then Left $ "Duplicate key(s) found: " <> pack (show duplicates)
        else eRowTable

joinKeyRowTable :: (Eq key, Ord key) => KeyRowTable key a -> KeyRowTable key a -> KeyRowTable key a
joinKeyRowTable table1 table2 =
    let newTable1 = addDefaultKeyRows table1 $ ((\\) `on` keyTableKeys) table2 table1
        newTable2 = addDefaultKeyRows table2 $ ((\\) `on` keyTableKeys) table1 table2
    in newTable1 `leftJoinKeyRowTable` newTable2
    where
        addDefaultKeyRows :: KeyRowTable key a -> [key] -> KeyRowTable key a
        addDefaultKeyRows (RowTable titles keyRows) newKeys = 
            let newKeyRows = (\key -> KeyRow (key, createDefaultRow titles)) <$> newKeys
            in RowTable titles (keyRows ++ newKeyRows)

leftJoinKeyRowTable :: (Eq key, Ord key) => KeyRowTable key a -> KeyRowTable key a -> KeyRowTable key a
leftJoinKeyRowTable (RowTable titles1 rows1) (RowTable titles2 rows2) =
    let rows2Map = Map.fromList [(fst tuple, KeyRow tuple) | (KeyRow tuple) <- rows2]
        newRows = getOrCreateRow titles2 rows2Map <$> [key | (KeyRow (key, _)) <- rows1]
    in RowTable titles1 rows1 <> RowTable titles2 newRows
    where
        getOrCreateRow :: Ord key => [Title a] -> Map.Map key (KeyRow key a) -> key -> KeyRow key a
        getOrCreateRow titles rowMap key = 
            let mRow = Map.lookup key rowMap
            in case mRow of
                Nothing -> KeyRow (key, createDefaultRow titles) 
                Just row -> row
        
-- | inner Fun
createDefaultRow :: [Title a] -> [a]
createDefaultRow titles = [emptyVal | Title _ emptyVal <- titles]

keyTableKeys :: KeyRowTable key a -> [key]
keyTableKeys (RowTable _ keyRows) = [key | KeyRow (key, _) <- keyRows]
