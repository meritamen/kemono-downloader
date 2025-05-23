{-# LANGUAGE OverloadedStrings #-}

module Kemono.Downloader.Utils where

import Data.Text

(</>) :: Text -> Text -> Text
txt1 </> txt2 = txt1 <> "/" <> txt2
