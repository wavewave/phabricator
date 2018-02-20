{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Phabricator.Differential where

import           Control.Lens          ((^.),makeLenses)
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Maybe            (fromMaybe)
import           Data.Monoid           ((<>))
import           Data.Text             (Text)
import qualified Data.Text        as T
import           GHC.Generics          (Generic)
import           Network.Wreq          (post,responseBody,FormParam((:=)))

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

makeLenses ''Field

instance FromJSON Field where
  parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = drop 7 })

instance ToJSON Field where
  toJSON = genericToJSON (defaultOptions { fieldLabelModifier = drop 7 })


data ItemType = DREV
              deriving (Show,Eq)

itemTypeToText :: ItemType -> Text
itemTypeToText DREV = "DREV"

textToItemType :: (Monad m) => Text -> m ItemType
textToItemType "DREV" = pure DREV
textToItemType x      = fail (T.unpack x  ++ " is not ItemType.")


instance ToJSON ItemType where
  toJSON = String . itemTypeToText

instance FromJSON ItemType where
  parseJSON (String txt) = textToItemType txt
  parseJSON invalid = typeMismatch "ItemType" invalid


data Item = Item { _item_attachments :: Value
                 , _item_phid :: Text
                 , _item_id :: Int
                 , _item_type :: ItemType
                 , _item_fields :: Field
                 }
              deriving (Show,Eq,Generic)

makeLenses ''Item

instance FromJSON Item where
  parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = drop 6 })

instance ToJSON Item where
  toJSON = genericToJSON (defaultOptions { fieldLabelModifier = drop 6 })



data QueryKey = Open
              | Active
              | Authored
              | All
              deriving (Show,Eq,Generic)

makeLenses ''QueryKey

queryKeyToText :: QueryKey -> Text
queryKeyToText Open     = "lks1dJdapQFa"
queryKeyToText Active   = "active"
queryKeyToText Authored = "authored"
queryKeyToText All      = "all"


textToQueryKey :: Monad m => Text -> m QueryKey
textToQueryKey txt = case txt of
                       "lks1dJdapQFa" -> pure Open
                       "active"       -> pure Active
                       "authored"     -> pure Authored
                       "all"          -> pure All
                       x              -> fail (T.unpack x ++ " is not QueryKey.")


instance ToJSON QueryKey where
  toJSON = String . queryKeyToText

instance FromJSON QueryKey where
  parseJSON (String txt) = textToQueryKey txt
  parseJSON invalid = typeMismatch "QueryKey" invalid

data PhabQuery = PhabQuery { _pq_queryKey :: QueryKey
                           -- , constraints :: Value
                           -- , attachments :: Value
                           -- , order :: Value
                           -- , before :: Value
                           -- , after :: Value
                           -- , limit :: Value
                           -- , OutputFormat :: Value
                           }
               deriving (Show,Eq,Generic)

makeLenses ''PhabQuery

instance FromJSON PhabQuery where
  parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = drop 4 })

instance ToJSON PhabQuery where
  toJSON = genericToJSON (defaultOptions { fieldLabelModifier = drop 4 })



{-
data Constraints = Constraints { ids :: Value
                               , phid :: Value
                               , responsiblePHIDs :: Value
                               , authorPHIDs :: Value
                               , reviewerPHIDs :: Value
                               , repositoryPHIDs :: Value
                               , statuses :: Value
                               , query :: Value
                               , subscribers :: Query
                               , projects :: Value
                               , bucket :: Value
                               }

data Order = Newest
           | Oldest
           | Relevance
           | Updated
           | Outdated


-}


data PhabResult = PhabResult { _pr_maps :: Value
                             , _pr_cursor :: Value
                             , _pr_data :: [Item]
                             , _pr_query :: PhabQuery
                             }
                deriving (Show,Eq,Generic)

makeLenses ''PhabResult

instance FromJSON PhabResult where
  parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = drop 4 })

instance ToJSON PhabResult where
  toJSON = genericToJSON (defaultOptions { fieldLabelModifier = drop 4})




data PhabResponse = PhabResponse { _pres_result :: PhabResult
                                 , _pres_error_code :: Maybe Int
                                 , _pres_error_info :: Maybe Text
                                 }
                  deriving (Show,Eq,Generic)

makeLenses ''PhabResponse

instance FromJSON PhabResponse where
  parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = drop 6 })


instance ToJSON PhabResponse where
  toJSON = genericToJSON (defaultOptions { fieldLabelModifier = drop 6})


type URL = Text

type Token = Text

runQuery :: URL -> Token -> PhabQuery -> IO (Either String PhabResponse)
runQuery url token query = do
  r <- post (T.unpack (url <> "/api/differential.revision.search"))
            ([ "api.token" := token
             , "queryKey" := queryKeyToText (_pq_queryKey query)
             -- , "limit" := (5 :: Int)
             ])
  return (eitherDecode (r ^. responseBody) :: Either String PhabResponse)
