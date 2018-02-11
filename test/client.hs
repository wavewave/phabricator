{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Encode.Pretty         (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Text                        (Text)
import           Network.Wreq
import           System.Environment               (getEnv)
--
import           Phabricator.Differential         (PhabResponse,PhabResult)

main = do
  putStrLn "query test"
  token <- getEnv "PHAB_API_TOKEN"
  
  r <- post "https://uphere.phacility.com/api/differential.revision.search"
            ([ "api.token" := token
             , "queryKey" := ("all" :: Text)
             , "limit" := (5 :: Int)
             
             ])
  let mv = eitherDecode (r ^. responseBody) :: Either String PhabResponse
  print mv
  -- mapM_ (BL.putStrLn . encodePretty) mv
