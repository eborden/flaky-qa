{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib (main) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Database.SQLite.Simple (Connection, Query, close, execute_, open)
import Database.SQLite.Simple.FromRow ()
import Web.Scotty (get, html, redirect, scotty)

main :: IO ()
main = do
  executeWithConn_ "CREATE TABLE IF NOT EXISTS flaky_event (created_at DATETIME DEFAULT CURRENT_TIMESTAMP NOT NULL)"

  scotty 8080 $ do
    get "/"
      $ html "<a href=\"/qa-was-flaky\">qa-was-flaky</a>"
    get "/qa-was-flaky" $ do
      liftIO $ executeWithConn_ "INSERT INTO flaky_event VALUES (CURRENT_TIMESTAMP)"
      redirect "/"

executeWithConn_ :: Query -> IO ()
executeWithConn_ query = do
  conn <- open "flaky_qa_stats.db"
  execute_ conn query
  close conn
