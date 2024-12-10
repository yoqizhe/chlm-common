{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}

module Support.Excel
( ExcelData(..)
, ReportCell(..)
, convertToExcelBS
, reportCell2cellValue
)
where

import Yesod.Core
import Codec.Xlsx
import Control.Lens
import Data.Decimal (Decimal)
import Data.Time.LocalTime (LocalTime)
import Data.Text (Text, pack)
import qualified Data.ByteString.Lazy as L
import Data.Time.Clock.POSIX
import Data.Time.Format (formatTime, defaultTimeLocale)
import Model.Decimal

class ExcelData a where

    excelContents :: a -> [[CellValue]]

data ReportCell = TextCell Text | NumCell Decimal | DateCell LocalTime

-- 为 ReportCell 实现 Num 实例
instance Num ReportCell where
    -- 加法：如果是两个 NumCell，执行加法运算
    (NumCell x) + (NumCell y) = NumCell (x + y)
    _ + _ = TextCell ""  -- 其他情况返回空文本

    -- 减法：如果是两个 NumCell，执行减法运算
    (NumCell x) - (NumCell y) = NumCell (x - y)
    _ - _ = TextCell ""  -- 其他情况返回空文本

    -- 乘法：如果是两个 NumCell，执行乘法运算
    (NumCell x) * (NumCell y) = NumCell (x * y)
    _ * _ = TextCell ""  -- 其他情况返回空文本

    -- 绝对值：如果是 NumCell，执行绝对值运算
    abs (NumCell x) = NumCell (abs x)
    abs _ = TextCell ""  -- 其他情况返回空文本

    -- 符号：如果是 NumCell，返回符号
    signum (NumCell x) = NumCell (signum x)
    signum _ = TextCell ""  -- 其他情况返回空文本

    -- 从整数转换：将整数转换为 NumCell 类型
    fromInteger x = NumCell (fromInteger x)

instance ToJSON ReportCell where
    toJSON (TextCell text) = toJSON text
    toJSON (NumCell num) = toJSON num
    toJSON (DateCell date) = toJSON (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" date)

reportCell2cellValue :: ReportCell -> CellValue
reportCell2cellValue (TextCell txt) = CellText txt
reportCell2cellValue (NumCell num) = CellDouble (realToFrac num)  -- 将 Decimal 转换为 Double
reportCell2cellValue (DateCell date) = CellText (pack (show date))  -- 将 LocalTime 转换为字符串

convertToExcelBS :: (MonadHandler m, ExcelData a) => Text -> a -> m L.ByteString
convertToExcelBS excelName excelData = do
    let sheet = def & wsCells .~ createCellMap excelData
        xlsx = def & atSheet excelName ?~ sheet
    ct <- liftIO getPOSIXTime
    return $ fromXlsx ct xlsx
    where
        createCellMap :: ExcelData a => a -> CellMap
        createCellMap excelData = fromRows $ contentRows excelData

        contentRows :: ExcelData a => a -> [(RowIndex, [(ColumnIndex, Cell)])]
        contentRows excelData =
            let rows = excelContents excelData
            in zipWith (\rowIndex row -> (RowIndex rowIndex, createRow row)) [1..] rows
            where
                createRow :: [CellValue] -> [(ColumnIndex, Cell)]
                createRow values = zipWith (\colIndex value -> (ColumnIndex colIndex, def & cellValue ?~ value)) [1..] values
