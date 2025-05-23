module Kemono.Downloader.Fetch where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Simple

fetchResponse :: (MonadThrow f, MonadIO f) => String -> f ByteString
fetchResponse url = fmap getResponseBody $ parseRequest url >>= httpLBS

fetchJson :: (FromJSON m, MonadThrow f, MonadIO f) => Text -> f (Maybe m)
fetchJson url = fmap decode . fetchResponse . T.unpack $ url
