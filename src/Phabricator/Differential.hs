{-# LANGUAGE DeriveGeneric #-}

module Phabricator.Differential where

import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)
import GHC.Generics (Generic)

data Field = Field { _field_summary :: Text
                   , _field_status :: Value
                   , _field_repositoryPHID :: Maybe Text
                   , _field_diffPHID :: Maybe Text
                   , _field_dateCreated :: Int
                   , _field_dateModified :: Int
                   , _field_authorPHID :: Text
                   , _field_title :: Text
                   , _field_policy :: Value
                   }
           deriving (Show,Eq,Generic)

instance FromJSON Field where
  parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = drop 7 })

instance ToJSON Field where
  toJSON = genericToJSON (defaultOptions { fieldLabelModifier = drop 7 })


data Item = Item { _item_attachments :: Value
                 , _item_phid :: Text
                 , _item_id :: Int
                 , _item_type :: Text
                 , _item_fields :: Field
                 }
              deriving (Show,Eq,Generic)

instance FromJSON Item where
  parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = drop 6 })

instance ToJSON Item where
  toJSON = genericToJSON (defaultOptions { fieldLabelModifier = drop 6 })



data PhabResult = PhabResult { _pr_maps :: Value
                             , _pr_cursor :: Value
                             , _pr_data :: [Item]
                             , _pr_query :: Value
                             }
                deriving (Show,Eq,Generic)

instance FromJSON PhabResult where
  parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = drop 4 })

instance ToJSON PhabResult where
  toJSON = genericToJSON (defaultOptions { fieldLabelModifier = drop 4})




data PhabResponse = PhabResponse { -- query :: Value
                                   result :: PhabResult
                                 , error_code :: Maybe Int
                                 , error_info :: Maybe Text
                                 }
                  deriving (Show,Eq,Generic)


instance FromJSON PhabResponse

instance ToJSON PhabResponse
