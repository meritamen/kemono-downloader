module Main where

import Options.Applicative

import ConvertUrl
import Downloader
import FetchJson

data Option = PostUrl String

postUrlP :: Parser Option
postUrlP = PostUrl <$> argument str
  (metavar "<URL>" <> help "Specify the Kemono post link to download.")

optionP :: Parser Option
optionP = postUrlP

getOption :: IO Option
getOption = execParser $ info (optionP <**> helper) $
  fullDesc <> progDesc "Kemono Downloader, Mrjt Jmn<meritamen@sdf.org>"

runOption :: Option -> IO ()
runOption (PostUrl url) = do
  case convertPageUrlToApi url of
    Nothing -> putStrLn "Invalid Kemono post URL format."
    Just url -> do
      json <- extractJson url
      case json of
        Nothing -> putStrLn "Invalid json format."
        Just result -> download result

main :: IO ()
main = getOption >>= runOption
