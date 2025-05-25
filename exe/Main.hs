module Main where

import qualified MyLib (parseAndPrint)
import Options.Applicative

data Command
  = AddCommand AddCommandOptions
  | ReportCommand ReportCommandOptions

data ReportCommandOptions = ReportCommandOptions
  { reportType :: String
  , reportFilter :: String
  }

data AddCommandOptions = AddCommandOptions
  { addDate :: String
  , addTitle :: String
  , addNote :: Maybe String
  , addCategory :: String
  , addAmount :: String
  } deriving (Show)

addCommandParser :: Parser AddCommandOptions
addCommandParser =
  AddCommandOptions
    <$> argument str (metavar "DATE")
    <*> strOption (long "title" <> metavar "TITLE")
    <*> optional (strOption (long "note" <> metavar "NOTE"))
    <*> strOption (long "category" <> metavar "CATEGORY")
    <*> strOption (long "amount" <> metavar "AMOUNT")

main :: IO ()
main = do
  let parserInfo = info addCommandParser (fullDesc <> progDesc "Add a new transaction")
  opts <- execParser parserInfo
  putStrLn "Parsed Add options: "
  print opts
  MyLib.parseAndPrint
