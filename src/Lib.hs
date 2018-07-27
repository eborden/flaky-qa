{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib (main) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Functor (void)
import Database.PostgreSQL.Simple (Connection, Query, close, connectPostgreSQL, execute_)
import Database.PostgreSQL.Simple.FromRow ()
import System.Environment (getEnv)
import Text.RawString.QQ (r)
import Web.Scotty (get, html, redirect, scotty)

main :: IO ()
main = do
  port <- read <$> getEnv "PORT"
  connStr <- pack <$> getEnv "DATABASE_URL"
  executeWithConn_ connStr "CREATE TABLE IF NOT EXISTS flaky_event (created_at timestamp with time zone DEFAULT now() NOT NULL)"

  scotty port $ do
    get "/" $ body False
    get "/thanks" $ body True
    get "/qa-was-flaky" $ do
      liftIO $ executeWithConn_ connStr "INSERT INTO flaky_event VALUES (now())"
      redirect "/thanks"


executeWithConn_ :: ByteString -> Query -> IO ()
executeWithConn_ connStr query = do
  conn <- connectPostgreSQL connStr
  execute_ conn query
  void $ close conn

body isThanks =
  html $ [r|
    <html>
      <head>
        <title>Was QA Flaky?</title>
        <style>
          body {
            background: #ddd;
            display: flex;
            align-items: center;
            justify-content: center;
            flex-flow: column;
          }
          a {
            border: 3px solid #fff;
            padding: 2em;
            text-size: 2em;
            border-radius: 0.5em;
            background: red;
            color: #fff;
            font-family: arial;
            text-decoration: none;
            font-weight: bold;
          }
          h1 {
          }
        </style>
      </head>
    <body>
    |] <>
    (if isThanks
      then [r|
        <h1>Thanks for Reporting</h1>
        |]
      else ""
    ) <>
    [r|
      <a href="/qa-was-flaky">QA was Flaky</a>
    </body>
    </html>
  |]
