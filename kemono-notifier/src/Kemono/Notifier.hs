{-# LANGUAGE OverloadedStrings #-}

module Kemono.Notifier where

import Control.Concurrent
import Control.Concurrent.Async
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Dhall

import Kemono.Notifier.Fetch
import Kemono.Notifier.Printer
import Kemono.Notifier.Types

creatorMonitor :: Creator -> IO ()
creatorMonitor (Creator service creatorId creatorName) = do
  posts <- fetchPosts service creatorId
  let seen = Set.fromList (map postId posts)
  ts <- timestamp
  TIO.putStrLn $ "[+] [" <> ts <> "] " <> creatorName <> " [" <> service <> "/" <> creatorId <> "] Monitor started."
  loop seen
  where
    loop seen = do
      posts <- fetchPosts service creatorId
      let newPosts = filter (\p -> postId p `Set.notMember` seen) posts
      ts <- timestamp
      if not (null newPosts)
        then do
          TIO.putStrLn $ "[+] [" <> ts <> "] " <> creatorName <> " [" <> service <> "/" <> creatorId <> "] Detected " <> T.pack (show (length newPosts)) <> " new post(s):"
          printNewPosts newPosts
          loop (Set.union seen (Set.fromList (map postId newPosts)))
        else do
          TIO.putStrLn $ "[-] [" <> ts <> "] " <> creatorName <> " [" <> service <> "/" <> creatorId <> "] No new posts."
          threadDelay (5 * 60 * 1000000)
          loop seen

monitor :: IO ()
monitor = do
  creators <- Dhall.input Dhall.auto "./creators.dhall" :: IO Creators
  TIO.putStrLn "[+] Kemono.su Notifier started."
  _ <- forConcurrently_ creators creatorMonitor
  return ()
