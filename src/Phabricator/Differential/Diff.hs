{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Phabricator.Differential.Diff where

import           Control.Lens          ((^.),makeLenses)
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Maybe            (fromMaybe)
import           Data.Monoid           ((<>))
import           Data.Text             (Text)
import qualified Data.Text        as T
import           GHC.Generics          (Generic)
--
import           Phabricator.Common


data Field = Field { _field_revisionPHID :: Maybe Text
                   , _field_authorPHID :: Text
                   , _field_repositoryPHID :: Maybe Text
                   , _field_refs :: Value
                   , _field_dateCreated :: Int
                   , _field_dateModified :: Int
                   , _field_policy :: Value
                   }
           deriving (Show,Eq,Generic)

makeLenses ''Field

instance FromJSON Field where
  parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = drop 7 })

instance ToJSON Field where
  toJSON = genericToJSON (defaultOptions { fieldLabelModifier = drop 7 })


data QueryKey = All
              deriving (Show,Eq,Generic)

makeLenses ''QueryKey

instance QueryKeyable QueryKey where
  queryKeyToText :: QueryKey -> Text
  queryKeyToText All      = "all"

  textToQueryKey :: Monad m => Text -> m QueryKey
  textToQueryKey txt = case txt of
                         "all"          -> pure All
                         x              -> fail (T.unpack x ++ " is not QueryKey.")


instance ToJSON QueryKey where
  toJSON = String . queryKeyToText

instance FromJSON QueryKey where
  parseJSON (String txt) = textToQueryKey txt
  parseJSON invalid = typeMismatch "QueryKey" invalid

data PhabResult = PhabResult { _pr_maps :: Value
                             , _pr_cursor :: Value
                             , _pr_data :: [Item Field]
                             , _pr_query :: PhabQuery QueryKey
                             }
                deriving (Show,Eq,Generic)

makeLenses ''PhabResult

instance FromJSON PhabResult where
  parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = drop 4 })

instance ToJSON PhabResult where
  toJSON = genericToJSON (defaultOptions { fieldLabelModifier = drop 4})

apiPoint :: Text
apiPoint = "/api/differential.diff.search"
