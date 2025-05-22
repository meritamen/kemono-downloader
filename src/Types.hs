{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

module Types where

import Data.Aeson
import Data.Text
import GHC.Generics

newtype Post = Post { title :: Text }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON

data FileEntry = FileEntry
  { server :: Text
  , name :: Text
  , path :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON

data APIResponse = APIResponse
  { post :: Post
  , previews :: [FileEntry]
  , attachments :: [FileEntry]
  }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON
