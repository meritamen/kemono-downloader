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
  putStrLn $ "Downloading: " <> filename
  response <- httpLBS =<< parseRequest url
  BS.writeFile outPath (getResponseBody response)

download :: APIResponse -> Bool -> IO ()
download apiResponse isSequentialFileName = do
  let
    targetDir = title . post $ apiResponse
    previewFiles = previews apiResponse
    attachmentFiles = attachments apiResponse
    allFiles = zip [1..] $ previewFiles <> attachmentFiles
  createDirectoryIfMissing True targetDir
  mapConcurrently_ (\(i, f) -> downloadFile
                               "."
                               (server f </> "data" <> path f)
                               (targetDir </> if isSequentialFileName
                                              then show i <> takeExtension (name f)
                                              else name  f))
                   allFiles
  putStrLn "All downloads completed."
