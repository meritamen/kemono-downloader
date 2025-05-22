{-# LANGUAGE OverloadedStrings #-}

module ConvertUrl where

import Data.Text (Text)
import qualified Data.Text as T

import Utils

kemonoApiPrefix :: Text
kemonoApiPrefix = "https://kemono.su/api/v1/"

convertPageUrlToApi :: Text -> Maybe Text
convertPageUrlToApi url =
  case T.stripPrefix "https://kemono.su/" url of
    Just rest ->
      let parts = T.splitOn "/" rest
      in case parts of
          [service, "user", userId, "post", postId] ->
            Just $ kemonoApiPrefix <> service </> "user" </> userId </> "post" </> postId
          _ -> Nothing
    Nothing -> Nothing
