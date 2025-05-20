{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import GHC.Generics
import Network.HTTP.Simple
import System.Directory
import System.Environment
import System.FilePath

data Post = Post { title :: String }
  deriving (Show, Generic)

deriving instance FromJSON Post

data FileEntry = FileEntry
  { server :: String
  , name :: String
  , path :: String
  }
  deriving (Show, Generic)

deriving instance FromJSON FileEntry

data APIResponse = APIResponse
  { post :: Post
  , previews :: [FileEntry]
  , attachments :: [FileEntry]
  }
  deriving (Show, Generic)

deriving instance FromJSON APIResponse

fetchResponse :: (MonadThrow f, MonadIO f) => String -> f ByteString
fetchResponse url = fmap getResponseBody $ parseRequest url >>= httpLBS

extractJson :: (MonadThrow f, MonadIO f) => String -> f (Maybe APIResponse)
extractJson url = decode <$> fetchResponse url

downloadFile :: FilePath -> String -> String -> IO ()
downloadFile dir url filename = do
  let outPath = dir </> filename
  putStrLn $ "Downloading: " ++ url
  response <- httpLBS =<< parseRequest url
  BS.writeFile outPath (getResponseBody response)

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
            <> T.unpack service <> "/user/" <> T.unpack userId <> "/post/" <> T.unpack postId
          _ -> Nothing
    Nothing -> Nothing

main :: IO ()
main = do
  url <- head <$> getArgs
  case convertPageUrlToApi url of
    Nothing -> putStrLn "Invalid Kemono post URL format."
    Just url -> do
      json <- extractJson url
      case json of
        Nothing -> putStrLn "Invalid json format."
        Just result -> do
          let targetDir = title . post $ result
              previewFiles = previews result
              attachmentFiles = attachments result
          createDirectoryIfMissing True targetDir
          downloadFiles previewFiles targetDir
          downloadFiles attachmentFiles targetDir
  where
    downloadFiles urls targetDir = forM_ urls $
      \f -> downloadFile
            "."
            (server f <> "/data/" <> path f)
            (targetDir </> name f)
