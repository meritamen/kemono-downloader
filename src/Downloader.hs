module Downloader where

import Control.Monad
import qualified Data.ByteString.Lazy as BS
import Network.HTTP.Simple
import System.Directory
import System.FilePath

import Types

downloadFile :: FilePath -> String -> String -> IO ()
downloadFile dir url filename = do
  let outPath = dir </> filename
  putStrLn $ "Downloading: " ++ url
  response <- httpLBS =<< parseRequest url
  BS.writeFile outPath (getResponseBody response)

download :: APIResponse -> IO ()
download apiResponse = do
  let
    targetDir = title . post $ apiResponse
    previewFiles = previews apiResponse
    attachmentFiles = attachments apiResponse
  createDirectoryIfMissing True targetDir
  downloadFiles previewFiles targetDir
  downloadFiles attachmentFiles targetDir
  where
    downloadFiles urls targetDir = forM_ urls $
      \f -> downloadFile
            "."
            (server f <> "/data/" <> path f)
            (targetDir </> name f)
