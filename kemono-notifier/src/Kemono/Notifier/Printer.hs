{-# LANGUAGE TypeApplications #-}

module Kemono.Notifier.Printer where

import qualified Data.Text as T

import Kemono.Notifier.Api
import Kemono.Notifier.Types

printNewPosts :: [Post] -> IO ()
printNewPosts posts =
    mapM_
    (\(i, p) -> putStrLn $
      "[" <> show @Int i
      <> "] "
      <> T.unpack (postTitle p)
      <> "\n    "
      <> T.unpack (postUrl p)
  ) $ zip [1..] posts
