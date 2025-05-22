module Main where

import Data.Text
import Options.Applicative

import ConvertUrl
import Downloader
import FetchJson

data Option = Option Text Bool

optionP :: Parser Option
optionP = Option
  <$> argument str (metavar "<URL>" <> help "Specify the Kemono post link to download.")
  <*> flag False True
        ( long "sequential_file"
       <> short 's'
       <> help "Use sequential numbering for downloaded file names." )

getOption :: IO Option
getOption = execParser $ info (optionP <**> helper) $
  fullDesc <> progDesc "Kemono Downloader, Mrjt Jmn<meritamen@sdf.org>"

runOption :: Option -> IO ()
runOption (Option url isSeq) = do
  case convertPageUrlToApi url of
    Nothing -> putStrLn "Invalid Kemono post URL format."
    Just url -> do
      json <- extractJson url
      case json of
        Nothing -> putStrLn "Invalid json format."
        Just result -> download result isSeq

main :: IO ()
main = getOption >>= runOption
