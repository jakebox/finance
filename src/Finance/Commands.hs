{-# LANGUAGE NamedFieldPuns #-}

module Finance.Commands (runReport, reportCommandParser, Command(..)) where

import Finance.Core
import Finance.Input
import Finance.Types

import Options.Applicative

newtype Command = ReportCommand ReportCommandOptions

data ReportCommandOptions = ReportCommandOptions
  { rType :: String
  , rCategory :: Maybe String
  -- , rFilters :: Maybe [Filter]
  }

reportCommandParser :: Parser ReportCommandOptions
reportCommandParser =
  ReportCommandOptions
    <$> argument str (metavar "TYPE")
    <*> optional (strOption (metavar "CATEGORY"))

runReport :: ReportCommandOptions -> IO ()
runReport ReportCommandOptions {rType, rCategory} = do
  let fp = "transactions.csv"
  txs <- readTransactionFile fp
  print txs

--     Left err -> error "Error processing file: " <> show err
--     Right txs -> do
--         let f = filterTransactions txs filterPs
--         case rType of
--           "category" -> spendingByCategory f
--           _ -> error "not implemented"
-- where
--   filterPs = undefined
