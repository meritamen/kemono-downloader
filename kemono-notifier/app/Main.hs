{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Data.Aeson
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import qualified Dhall
import GHC.Generics
import Network.HTTP.Simple

data Creator = Creator
  { service :: Text
  , id      :: Text
  } deriving (Show, Eq, Generic)

instance Dhall.FromDhall Creator

type Creators = [Creator]

data Post = Post
  { postId    :: Text
  , postTitle :: Text
  , postUser  :: Text
  , postService :: Text
  } deriving (Show, Eq, Ord)

instance FromJSON Post where
  parseJSON = withObject "Post" $ \v ->
    Post <$> v .: "id"
         <*> v .:? "title" .!= ""
         <*> v .: "user"
         <*> v .: "service"

apiUrl :: Text -> Text -> Text
apiUrl service creatorId =
  "https://kemono.su/api/v1/"
    <> service
    <> "/user/"
    <> creatorId

postLink :: Post -> Text
postLink p =
  "https://kemono.su/"
    <> postService p
    <> "/user/"
    <> postUser p
    <> "/post/"
    <> postId p

timestamp :: IO Text
timestamp = do
  now <- getCurrentTime
  timezone <- getCurrentTimeZone
  let local = utcToLocalTime timezone now
  return . T.pack $
    formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" local

logMsg :: Text -> IO ()
logMsg msg = do
  ts <- timestamp
  putStrLn . T.unpack $
    msg <> " [" <> ts <> "]"

fetchPosts :: Text -> Text -> IO [Post]
fetchPosts service creatorId = do
  let url = T.unpack $ apiUrl service creatorId
  response <- httpLBS (parseRequest_ url)
  let body = getResponseBody response
  case eitherDecode body of
    Left err -> do
      logMsg $
        "[-] Failed to parse JSON from "
        <> service
        <> "/" <> creatorId
        <> ": " <> T.pack err
      return []
    Right posts -> return posts

printNewPosts :: [Post] -> IO ()
printNewPosts posts =
    mapM_
    (\(i, p) -> putStrLn $
      "[" <> show @Int i
      <> "] "
      <> T.unpack (postTitle p)
      <> "\n    "
      <> T.unpack (postLink p)
  ) $ zip [1..] posts

monitorCreator :: Creator -> IO ()
monitorCreator (Creator service creatorId) = do
  posts <- fetchPosts service creatorId
  let seen = Set.fromList (map postId posts)
  ts <- timestamp
  putStrLn . T.unpack $
    "[+] [" <> ts <> "] [" <> service <> "/" <> creatorId <> "] Monitor started."
  loop seen
  where
    loop seen = do
      posts <- fetchPosts service creatorId
      let newPosts =
            filter (\p -> postId p `Set.notMember` seen) posts
      ts <- timestamp
      if not (null newPosts)
        then do
          putStrLn . T.unpack $
            "[+] [" <> ts <> "] ["
            <> service <> "/" <> creatorId
            <> "] Detected "
            <> T.pack (show (length newPosts))
            <> " new post(s):"
          printNewPosts newPosts
          loop (Set.union seen (Set.fromList (map postId newPosts)))
        else do
          putStrLn . T.unpack $
            "[-] [" <> ts <> "] ["
            <> service <> "/" <> creatorId
            <> "] No new posts."
          threadDelay (5 * 60 * 1000000)
          loop seen

main :: IO ()
main = do
  creators <- Dhall.input Dhall.auto "./creators.dhall" :: IO Creators
  putStrLn "[+] Kemono.su Notifier started."
  _ <- forConcurrently_ creators monitorCreator
  return ()
