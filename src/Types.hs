{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Aeson
import GHC.Generics

data Post = Post { title :: String }
  deriving (Show, Generic, FromJSON)

data FileEntry = FileEntry
  { server :: String
  , name :: String
  , path :: String
  }
  deriving (Show, Generic, FromJSON)

data APIResponse = APIResponse
  { post :: Post
  , previews :: [FileEntry]
  , attachments :: [FileEntry]
  }
  deriving (Show, Generic, FromJSON)
