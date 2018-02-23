{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Phabricator.Query where

import           Control.Lens          ((^.))
import           Data.Aeson            (FromJSON(..),eitherDecode)
import           Data.Maybe            (maybe)
import           Data.Monoid           ((<>))
import           Data.Text             (Text)
import qualified Data.Text        as T
import           Network.Wreq          (post,responseBody,FormParam((:=)))
--
import           Phabricator.Common

import           Network.Wreq.Types    (postPayload)
import qualified Network.HTTP.Client as N


type URL = Text

type API = Text

type Token = Text

runQuery :: (ToFormParam q, ToFormParam c, FromJSON r) => URL -> API -> Token -> PhabQuery q c -> IO (Either String (PhabResponse r))
runQuery url api token query = do
  let params = ([ "api.token" := token ] ++ encodeFormParam (_pq_queryKey query) ++ maybe [] encodeFormParam (_pq_constraints query))
  {- r <- postPayload params N.defaultRequest
  case N.requestBody r of
    N.RequestBodyLBS str -> print str
    N.RequestBodyBS str -> print str
    _ -> return () -}
  r <- post (T.unpack (url <> api)) params
  -- print r
  return (eitherDecode (r ^. responseBody))

