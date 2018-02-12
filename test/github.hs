{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as B
import           GitHub.Auth
import qualified GitHub.Endpoints.Users.Followers as GitHub
import qualified GitHub.Endpoints.Repos as GitHub
import           System.Environment (getEnv)

main0 :: IO ()
main0 = do
  possibleUsers <- GitHub.usersFollowing "wavewave"
  print possibleUsers

main = do
  tok <- getEnv "GITHUB_TOKEN"
  let auth = OAuth (B.pack tok)
  possibleBranches <- GitHub.branchesFor' (Just auth) "wavewave" "dotfiles"
  print possibleBranches
