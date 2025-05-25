module Main where

import MyLib
  ( AddCommandOptions (..)
  , Command (..)
  , ReportCommandOptions (..)
  , parseAdd
  , parseReport
  )
import Options.Applicative

addCommandParser :: Parser AddCommandOptions
addCommandParser =
  AddCommandOptions
    <$> argument str (metavar "DATE" <> help "Date in YYYY-MM-DD format")
    <*> strOption (long "title" <> metavar "TITLE" <> help "Title of the transaction")
    <*> optional (strOption (long "note" <> metavar "NOTE" <> help "Optional note for the transaction"))
    <*> strOption (long "category" <> metavar "CATEGORY" <> help "Category of the transaction")
    <*> strOption (long "amount" <> metavar "AMOUNT" <> help "Amount of the transaction")

reportCommandParser :: Parser ReportCommandOptions
reportCommandParser =
  ReportCommandOptions
    <$> argument str (metavar "TYPE" <> help "Type of report to generate")
    <*> optional
      ( strOption
          ( long "date-block"
              <> metavar "FILTER"
              <> help "One of month or year or all, default all"
          )
      )
    <*> optional (strOption (long "category" <> metavar "CATEGORY" <> help "Filter by category"))

mainParser :: Parser Command
mainParser =
  subparser
    ( command
        "add"
        (info (helper <*> (AddCommand <$> addCommandParser)) (progDesc "Add a new transaction"))
        <> command
          "report"
          (info (helper <*> (ReportCommand <$> reportCommandParser)) (progDesc "Generate a report"))
    )

main :: IO ()
main = do
  cmd <-
    execParser (info mainParser (fullDesc <> progDesc "Financial Tracker CLI"))
  case cmd of
    AddCommand opts -> parseAdd opts
    ReportCommand opts -> parseReport opts
