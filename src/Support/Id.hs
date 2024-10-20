{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Support.Id where

import qualified Data.UUID.V4 as UUIDv4
import Data.UUID (UUID, toText)
import qualified Data.Text as T
import Data.Functor ((<&>))

generateUUID :: IO T.Text
generateUUID = UUIDv4.nextRandom <&> (T.filter (/= '-') . toText)
