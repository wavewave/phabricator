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
import qualified Phabricator.Differential   as D
import qualified Phabricator.Diffusion      as R

main = do
  putStrLn "query test"
  token <- T.pack <$> getEnv "PHAB_API_TOKEN"
  -- r <- D.runQuery "https://uphere.phacility.com" token D.PhabQuery { _pq_queryKey = Open }
  -- print r
  r <- R.runQuery "https://uphere.phacility.com" token R.PhabQuery { R._pq_queryKey = R.Active }
  print r
