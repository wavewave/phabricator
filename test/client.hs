{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import qualified Phabricator.Common         as C
import qualified Phabricator.Differential.Revision as D
import qualified Phabricator.Differential.Diff as DD
import qualified Phabricator.Diffusion      as R
import           Phabricator.Query

main = do
  putStrLn "query test"
  token <- T.pack <$> getEnv "PHAB_API_TOKEN"
  -- r <- D.runQuery "https://uphere.phacility.com" token D.PhabQuery { _pq_queryKey = Open }
  -- print r
  {- r :: Either String (C.PhabResponse R.PhabResult)
    <- runQuery "https://uphere.phacility.com" R.apiPoint token C.PhabQuery { C._pq_queryKey = R.Active }
  print r
  -}
  
  r :: Either String (C.PhabResponse DD.PhabResult)
    <- runQuery "https://uphere.phacility.com" DD.apiPoint token C.PhabQuery { C._pq_queryKey = DD.All }
  print r
