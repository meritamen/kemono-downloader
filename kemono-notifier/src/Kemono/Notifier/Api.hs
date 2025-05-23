{-# LANGUAGE OverloadedStrings #-}

module Kemono.Notifier.Api where

import Data.Text

import Kemono.Notifier.Types

apiUrl :: Text -> Text -> Text
apiUrl service creatorId =
  "https://kemono.su/api/v1/" <> service <> "/user/" <> creatorId

postUrl :: Post -> Text
postUrl p =
  "https://kemono.su/" <> postService p <> "/user/" <> postUser p <> "/post/" <> postId p
