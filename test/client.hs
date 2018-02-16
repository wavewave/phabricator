{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Encode.Pretty         (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Text                        (Text)
import qualified Data.Text                  as T
import           Network.Wreq
import           System.Environment               (getEnv)
--
import           Phabricator.Differential         (QueryKey(..),PhabQuery(..)
                                                  ,PhabResponse,PhabResult,runQuery)

main = do
  putStrLn "query test"
  token <- T.pack <$> getEnv "PHAB_API_TOKEN"
  runQuery "https://uphere.phacility.com" token PhabQuery { _pq_queryKey = Open }
