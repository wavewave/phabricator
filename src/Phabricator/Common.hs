{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Phabricator.Common where

import           Control.Lens          ((^.),makeLenses)
import           Data.Aeson            (FromJSON(..),ToJSON(..),defaultOptions
                                       ,genericParseJSON,genericToJSON)
import           Data.Aeson.Types      (fieldLabelModifier)
import           Data.Text             (Text)
import           GHC.Generics          (Generic)


data PhabResponse a =
  PhabResponse { _pres_result :: a
               , _pres_error_code :: Maybe Int
               , _pres_error_info :: Maybe Text
               }
  deriving (Show,Eq,Generic)

makeLenses ''PhabResponse

instance FromJSON a => FromJSON (PhabResponse a) where
  parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = drop 6 })


instance ToJSON a => ToJSON (PhabResponse a) where
  toJSON = genericToJSON (defaultOptions { fieldLabelModifier = drop 6})
