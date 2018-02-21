{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Phabricator.Differential.Revision where

import           Control.Lens          ((^.),makeLenses)
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Maybe            (fromMaybe)
import           Data.Monoid           ((<>))
import           Data.Text             (Text)
import qualified Data.Text        as T
import           GHC.Generics          (Generic)
import           Network.Wreq          (FormParam((:=)))
--
import           Phabricator.Common


data Field = Field { _field_summary :: Text
                   , _field_status :: Value
                   , _field_repositoryPHID :: Maybe Text
                   , _field_diffPHID :: Text
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

instance ToFormParam QueryKey where
  encodeFormParam q = [ "queryKey" := queryKeyToText q ]


instance ToJSON QueryKey where
  toJSON = String . queryKeyToText

instance FromJSON QueryKey where
  parseJSON (String txt) = textToQueryKey txt
  parseJSON invalid = typeMismatch "QueryKey" invalid


{-

data Order = Newest
           | Oldest
           | Relevance
           | Updated
           | Outdated


-}

data QueryConstraint =
  QueryConstraint { _qc_ids :: [Int]
                  , _qc_phid :: [Text]
                  , _qc_responsiblePHIDs ::[Text]
                  , _qc_authorPHIDs :: [Text]
                  , _qc_reviewerPHIDs :: [Text]
                  , _qc_repositoryPHIDs :: [Text]
                  , _qc_statuses :: [Text]
                  , _qc_query :: Text
                  , _qc_subscribers :: [Text]
                  , _qc_projects :: [Text]
                  -- , _qc_bucket :: Value
                  }
  deriving (Show,Eq,Generic)

makeLenses ''QueryConstraint

instance FromJSON QueryConstraint where
  parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = drop 4 })

instance ToJSON QueryConstraint where
  toJSON = genericToJSON (defaultOptions { fieldLabelModifier = drop 4})



data PhabResult = PhabResult { _pr_maps :: Value
                             , _pr_cursor :: Value
                             , _pr_data :: [Item Field]
                             , _pr_query :: PhabQuery QueryKey QueryConstraint
                             }
                deriving (Show,Eq,Generic)

makeLenses ''PhabResult

instance FromJSON PhabResult where
  parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = drop 4 })

instance ToJSON PhabResult where
  toJSON = genericToJSON (defaultOptions { fieldLabelModifier = drop 4})

apiPoint :: Text
apiPoint = "/api/differential.revision.search"




