{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Support.DateTime where

import Yesod.Core
import Data.Time.LocalTime
import Data.Time

-- Convert UTCTime to local time
convertToLocalTime :: UTCTime -> IO LocalTime
convertToLocalTime utcTime = do
    timeZone <- getCurrentTimeZone
    return $ utcToLocalTime timeZone utcTime

nowDateTime :: (MonadHandler m) => m LocalTime
nowDateTime = do
    utcNow <- liftIO getCurrentTime
    localNow <- liftIO $ convertToLocalTime utcNow
    return localNow

nowDay :: (MonadHandler m) => m Day
nowDay = do
    now <- nowDateTime
    return $ localDay now

data DayRange = DayRange
  { startDay :: Day
  , endDay :: Day
  }

createValidDayRange :: Day -> Day -> Maybe DayRange
createValidDayRange startDay endDay
  | startDay < endDay = Just DayRange { startDay = startDay, endDay = endDay }
  | otherwise         = Nothing

-- createDayRange :: Day -> Day -> DayRange
-- createDayRange startDay endDay = DayRange { startDay = startDay, endDay = endDay }

localTimeAddDay :: LocalTime -> Int -> LocalTime
localTimeAddDay localDateTime n =
  let day = localDay localDateTime
      newDay = addDays (fromIntegral n) day
  in localDateTime { localDay = newDay }