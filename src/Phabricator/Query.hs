{-# LANGUAGE OverloadedStrings #-}

module Phabricator.Query where

import           Control.Lens          ((^.))
import           Data.Aeson            (FromJSON(..),eitherDecode)
import           Data.Monoid           ((<>))
import           Data.Text             (Text)
import qualified Data.Text        as T
import           Network.Wreq          (post,responseBody,FormParam((:=)))
--
import           Phabricator.Common

type URL = Text

type API = Text

type Token = Text

runQuery :: (QueryKeyable q, FromJSON r) => URL -> API -> Token -> PhabQuery q -> IO (Either String (PhabResponse r))
runQuery url api token query = do
  r <- post (T.unpack (url <> api))
            ([ "api.token" := token
             , "queryKey" := queryKeyToText (_pq_queryKey query)
             -- , "limit" := (5 :: Int)
             ])
  return (eitherDecode (r ^. responseBody))
