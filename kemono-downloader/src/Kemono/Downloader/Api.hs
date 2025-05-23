{-# LANGUAGE OverloadedStrings #-}

module Kemono.Downloader.Api where

import Data.Text (Text)
import qualified Data.Text as T

import Kemono.Downloader.Utils

apiPrefix :: Text
apiPrefix = "https://kemono.su/api/v1/"

apiUrl :: Text -> Maybe Text
apiUrl url =
  case T.stripPrefix "https://kemono.su/" url of
    Just rest ->
      let parts = T.splitOn "/" rest
      in case parts of
          [service, "user", userId, "post", postId] ->
            Just $ apiPrefix <> service </> "user" </> userId </> "post" </> postId
          _ -> Nothing
    Nothing -> Nothing
