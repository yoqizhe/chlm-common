{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}
module Model.LocalTime where

import ClassyPrelude.Yesod
import Database.Persist.Sql (PersistField(..), PersistFieldSql(..), SqlType(SqlString))
import Data.Text (Text, pack, unpack)
import Data.Typeable (Typeable)
import Text.Read (readMaybe)
import qualified Data.Text as T
import Data.Time.LocalTime (LocalTime)
import Data.Aeson (FromJSON(..), Value(..), withScientific)
import Data.Time

fuckConvertToLocalTime :: UTCTime -> LocalTime
fuckConvertToLocalTime utcTime = utcToLocalTime utc utcTime

-- Add LocalTime instances for PersistField and PersistFieldSql
instance PersistField LocalTime where
    toPersistValue t = PersistText $ T.pack $ show t
    fromPersistValue (PersistUTCTime t) =
      let localTime = fuckConvertToLocalTime t
      in Right localTime
    fromPersistValue _ = Left "LocalTime must be converted from PersistText"

instance PersistFieldSql LocalTime where
    sqlType _ = SqlString
