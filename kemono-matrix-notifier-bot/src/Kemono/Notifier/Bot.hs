{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Kemono.Notifier.Bot where

import Control.Concurrent
import Control.Concurrent.Async
import qualified Dhall as Dhall
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Kemono.Notifier.Api
import Kemono.Notifier.Fetch
import Kemono.Notifier.Types
import Kemono.Notifier.Printer
import qualified Network.Matrix.Client as Matrix

send :: Matrix.ClientSession -> Matrix.RoomID -> Text -> IO ()
send session room body = do
  let roomMessage =
        Matrix.RoomMessageText
          ( Matrix.MessageText
              { Matrix.mtBody = body,
                Matrix.mtType = Matrix.TextType,
                Matrix.mtFormat = Nothing,
                Matrix.mtFormattedBody = Nothing
              }
          )
      txnId = "notify-" <> T.take 10 (T.replace ":" "_" body)
  ts <- timestamp
  eventID <- Matrix.sendMessage session room (Matrix.EventRoomMessage roomMessage) (Matrix.TxnID txnId)
  case eventID of
    Right eventID -> TIO.putStrLn $ "[+] [" <> ts <> "] " <> Matrix.unEventID eventID
    Left err -> TIO.putStrLn $ "[-] [" <> ts <> "] " <> T.pack (show err)

monitor :: Matrix.ClientSession -> Matrix.RoomID -> Creator -> IO ()
monitor session room (Creator creatorService creatorId creatorName) = do
  posts <- fetchPosts creatorService creatorId
  let seen = Set.fromList (map postId posts)
  ts <- timestamp
  TIO.putStrLn $ "[+] [" <> ts <> "] " <> creatorName <> " [" <> creatorService <> "/" <> creatorId <> "] Monitor started."
  loop seen
  where
    loop seen = do
      posts <- fetchPosts creatorService creatorId
      let newPosts =
            filter (\p -> postId p `Set.notMember` seen) posts
      ts <- timestamp
      if not (null newPosts)
        then do
          TIO.putStrLn $
            "[+] [" <> ts <> "] " <> creatorName <> " [" <> creatorService <> "/" <> creatorId <> "] Detected " <> T.pack (show (length newPosts)) <> " new post(s):"
          printNewPosts newPosts
          let messages = map (\p -> postTitle p <> "\n" <> postUrl p) newPosts
          mapM_ (send session room) messages
          loop (Set.union seen (Set.fromList (map postId newPosts)))
        else do
          TIO.putStrLn $
            "[-] [" <> ts <> "] " <> creatorService <> "/" <> creatorId <> "] No new posts."
          threadDelay (5 * 60 * 1000000)
          loop seen

bot :: IO ()
bot = do
  TIO.putStrLn "MatrixBot with Kemono Notifier starting..."
  config <- Dhall.input (Dhall.auto @Text) "./config.dhall"
  creators <- Dhall.input (Dhall.auto @[Creator]) "./creators.dhall"
  session <- Matrix.createSession "https://matrix.org" =<< Matrix.getTokenFromEnv "MATRIX_TOKEN"
  roomId <- Matrix.joinRoom session config
  case roomId of
    Right roomId -> forConcurrently_ creators (monitor session roomId)
    Left err -> print err
