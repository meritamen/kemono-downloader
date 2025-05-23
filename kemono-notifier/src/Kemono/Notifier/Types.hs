{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Kemono.Notifier.Types where

import Data.Aeson
import Dhall

data Creator = Creator
  { service :: Text
  , id :: Text
  , name :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass FromDhall

type Creators = [Creator]

data Post = Post
  { postId :: Text
  , postTitle :: Text
  , postUser :: Text
  , postService :: Text
  }
  deriving stock (Show, Eq, Ord)

instance FromJSON Post where
  parseJSON = withObject "Post" $ \v ->
    Post <$> v .: "id"
         <*> v .:? "title" .!= ""
         <*> v .: "user"
         <*> v .: "service"
