{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Downloader where

import Control.Concurrent.Async
import qualified Data.ByteString.Lazy as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Network.HTTP.Simple
import System.Directory
import System.FilePath
import TextShow

import Types
import qualified Utils as U

downloadFile :: FilePath -> Text -> Text -> IO ()
downloadFile dir url filename = do
  let outPath = dir </> T.unpack filename
  TIO.putStrLn $ "Downloading: " <> filename
  response <- httpLBS =<< parseRequest (T.unpack url)
  BS.writeFile outPath (getResponseBody response)

download :: APIResponse -> Bool -> IO ()
download apiResponse isSequentialFileName = do
  let
    targetDir = title . post $ apiResponse
    previewFiles = previews apiResponse
    attachmentFiles = attachments apiResponse
    allFiles = zip [1..] $ previewFiles <> attachmentFiles
  createDirectoryIfMissing True $ T.unpack targetDir
  mapConcurrently_
    (\(i, f) ->
        downloadFile
        "."
        (server f U.</> "data" <> path f)
        (targetDir U.</> if isSequentialFileName
                         then showt @Int i <> (T.pack . takeExtension . T.unpack . name $ f)
                         else name f))
    allFiles
  putStrLn "All downloads completed."
