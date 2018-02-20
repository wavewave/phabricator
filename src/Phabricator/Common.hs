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
import           Network.Wreq.Types    (FormParam(..))


data ItemType = DIFF
              | DREV
              | REPO
              deriving (Show,Eq)

itemTypeToText :: ItemType -> Text
itemTypeToText DIFF = "DIFF"
itemTypeToText DREV = "DREV"
itemTypeToText REPO = "REPO"


textToItemType :: (Monad m) => Text -> m ItemType
textToItemType "DIFF" = pure DIFF
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


class ToFormParam q where
  encodeFormParam :: q -> [FormParam]

{-
  queryKeyToText :: q -> Text
  textToQueryKey :: (Monad m) => Text -> m q
-}

data PhabQuery q c = PhabQuery { _pq_queryKey :: q
                               , _pq_constraints :: Maybe c
                               -- , attachments :: Value
                               -- , order :: Value
                               -- , before :: Value
                               -- , after :: Value
                               -- , limit :: Value
                               -- , OutputFormat :: Value
                               }
                 deriving (Show,Eq,Generic)

makeLenses ''PhabQuery

instance (FromJSON q, FromJSON c) => FromJSON (PhabQuery q c) where
  parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = drop 4 })

instance (ToJSON q, ToJSON c) => ToJSON (PhabQuery q c) where
  toJSON = genericToJSON (defaultOptions { fieldLabelModifier = drop 4 })




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
