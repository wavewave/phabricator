{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Phabricator.Diffusion where

import           Control.Lens          ((^.),makeLenses)
import           Data.Aeson
import           Data.Aeson.Types      (fieldLabelModifier,typeMismatch)
import           Data.Monoid           ((<>))
import           Data.Text             (Text)
import qualified Data.Text        as T
import           GHC.Generics          (Generic)
import           Network.Wreq          (post,responseBody,FormParam((:=)))
--
import           Phabricator.Common


data Field = Field { _field_name :: Text
                   , _field_vcs :: Text
                   , _field_callsign :: Maybe Text
                   , _field_shortName :: Maybe Text
                   , _field_status :: Text
                   , _field_isImporting :: Bool
                   , _field_spacePHID :: Maybe Text
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



data QueryKey = Active
              | All
              deriving (Show,Eq,Generic)

makeLenses ''QueryKey

instance QueryKeyable QueryKey where
  queryKeyToText :: QueryKey -> Text
  queryKeyToText Active   = "active"
  queryKeyToText All      = "all"

  textToQueryKey :: Monad m => Text -> m QueryKey
  textToQueryKey txt = case txt of
                         "active"       -> pure Active
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
apiPoint = "/api/diffusion.repository.search"

{-
type URL = Text

type Token = Text

runQuery :: URL -> Token -> PhabQuery -> IO (Either String (PhabResponse PhabResult))
runQuery url token query = do
  r <- post (T.unpack (url <> "/api/diffusion.repository.search"))
            ([ "api.token" := token
             , "queryKey" := queryKeyToText (_pq_queryKey query)
             -- , "limit" := (5 :: Int)
             ])
  return (eitherDecode (r ^. responseBody))
-}
