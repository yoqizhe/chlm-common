{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE DefaultSignatures #-}

module Support.Dict where

import qualified Data.Aeson as J
import Data.Aeson (ToJSON, toJSON, (.=), object)
import Data.Text (Text)

class (Enum a) => DictEnum a where
  dictLabel :: a -> Text
  dictValues :: [a]
  default dictValues :: (Enum a) => [a]
  dictValues = enumFrom (toEnum 0)

instance (DictEnum a) => ToJSON a where
  toJSON val = object [ "label" .= dictLabel val
                      , "value" .= fromEnum val
                      ]
