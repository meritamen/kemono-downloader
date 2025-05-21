module Downloader where

import Control.Concurrent.Async
import qualified Data.ByteString.Lazy as BS
import Network.HTTP.Simple
import System.Directory
import System.FilePath

import Types

downloadFile :: FilePath -> String -> String -> IO ()
downloadFile dir url filename = do
  let outPath = dir </> filename
  putStrLn $ "Downloading: " ++ filename
  response <- httpLBS =<< parseRequest url
  BS.writeFile outPath (getResponseBody response)

download :: APIResponse -> IO ()
download apiResponse = do
  let
    targetDir = title . post $ apiResponse
    previewFiles = previews apiResponse
    attachmentFiles = attachments apiResponse
    allFiles = previewFiles <> attachmentFiles
  createDirectoryIfMissing True targetDir
  mapConcurrently_ (\f -> downloadFile
                            "."
                            (server f </> "data" <> path f)
                            (targetDir </> name f)) allFiles
  putStrLn "All downloads completed."
