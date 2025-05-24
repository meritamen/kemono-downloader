module Coomer.Downloader where

import Data.Text (Text)

import Coomer.Downloader.Api
import Kemono.Downloader
import Kemono.Downloader.Fetch

handle :: Text -> Bool -> IO ()
handle url isSeq = do
  case apiUrl url of
    Nothing -> putStrLn "Invalid Coomer post URL format."
    Just url -> do
      json <- fetchJson url
      case json of
        Nothing -> putStrLn "Invalid json format."
        Just result -> download result isSeq
