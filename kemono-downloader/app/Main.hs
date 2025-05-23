module Main where

import Data.Text
import Options.Applicative

import Kemono.Downloader

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
runOption (Option url isSeq) = handle url isSeq

main :: IO ()
main = getOption >>= runOption
