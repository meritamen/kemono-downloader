{-# LANGUAGE OverloadedStrings #-}

module ConvertUrl where

import qualified Data.Text as T
import System.FilePath

kemonoApiPrefix :: String
kemonoApiPrefix = "https://kemono.su/api/v1/"

convertPageUrlToApi :: String -> Maybe String
convertPageUrlToApi url =
  case T.stripPrefix "https://kemono.su/" (T.pack url) of
    Just rest ->
      let parts = T.splitOn "/" rest
      in case parts of
          [service, "user", userId, "post", postId] ->
            Just $ kemonoApiPrefix
            <> T.unpack service </> "user" </> T.unpack userId </> "post" </> T.unpack postId
          _ -> Nothing
    Nothing -> Nothing
