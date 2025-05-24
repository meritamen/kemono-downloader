{-# LANGUAGE OverloadedStrings #-}

module Kemono.Notifier.Fetch where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time
import Network.HTTP.Simple

import Kemono.Notifier.Api
import Kemono.Notifier.Types

fetchPosts :: Text -> Text -> IO [Post]
fetchPosts service creatorId = do
  let url = T.unpack $ apiUrl service creatorId
  response <- httpLBS (parseRequest_ url)
  let body = getResponseBody response
  case eitherDecode body of
    Left err -> do
      ts <- timestamp
      TIO.putStrLn $
        "[-] [" <> ts <> "] Failed to parse JSON from " <> service <> "/" <> creatorId <> ": " <> T.pack err
      return []
    Right posts -> return posts

timestamp :: IO Text
timestamp = do
  now <- getCurrentTime
  timezone <- getCurrentTimeZone
  let local = utcToLocalTime timezone now
  return . T.pack $
    formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" local
