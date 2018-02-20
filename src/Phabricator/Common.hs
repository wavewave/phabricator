{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Phabricator.Common where

import           Control.Lens          ((^.),makeLenses)
import           Data.Aeson            (Value(..),FromJSON(..),ToJSON(..),defaultOptions
                                       ,genericParseJSON,genericToJSON)
import           Data.Aeson.Types      (fieldLabelModifier,typeMismatch)
import           Data.Text             (Text)
import qualified Data.Text        as T
import           GHC.Generics          (Generic)


data ItemType = DREV
              | REPO
              deriving (Show,Eq)

itemTypeToText :: ItemType -> Text
itemTypeToText DREV = "DREV"
itemTypeToText REPO = "REPO"


textToItemType :: (Monad m) => Text -> m ItemType
textToItemType "DREV" = pure DREV
textToItemType "REPO" = pure REPO
textToItemType x      = fail (T.unpack x  ++ " is not ItemType.")


instance ToJSON ItemType where
  toJSON = String . itemTypeToText

instance FromJSON ItemType where
  parseJSON (String txt) = textToItemType txt
  parseJSON invalid = typeMismatch "ItemType" invalid


data Item a = Item { _item_attachments :: Value
                   , _item_phid :: Text
                   , _item_id :: Int
                   , _item_type :: ItemType
                   , _item_fields :: a
                   }
            deriving (Show,Eq,Generic)

makeLenses ''Item

instance FromJSON a => FromJSON (Item a) where
  parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = drop 6 })

instance ToJSON a => ToJSON (Item a) where
  toJSON = genericToJSON (defaultOptions { fieldLabelModifier = drop 6 })


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
