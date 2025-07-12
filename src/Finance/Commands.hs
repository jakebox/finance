{-# LANGUAGE NamedFieldPuns #-}

module Finance.Commands (runReport, reportCommandParser, Command(..)) where

import Finance.Core
import Finance.Input
import Finance.Types
import Finance.PrettyPrint
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Options.Applicative


transactionsFile :: FilePath = "transactions.csv"

newtype Command = ReportCommand ReportCommandOptions

data ReportCommandOptions = ReportCommandOptions
  { rType :: String
--  , rCategory :: Maybe String
  , rFilters :: Maybe String
  }

reportCommandParser :: Parser ReportCommandOptions
reportCommandParser =
  ReportCommandOptions
    <$> argument str (metavar "TYPE")
--    <*> optional (argument str (metavar "CATEGORY"))
    <*> optional (argument str (metavar "FILTERS"))

runReport :: ReportCommandOptions -> IO ()
runReport ReportCommandOptions {rType, rFilters} = do
  txs <- readTransactionFile transactionsFile
  let filtered_txs = filterTransactions txs (stringToFilters rFilters)
  case rType of
    "summary" -> T.putStrLn . ppAggregatedSpending $ spendingByCategory filtered_txs
    -- "category" ->
    --   case rCategory of
    --     Just category -> print $ subcategories category . spendingByPurchaseCategory $ txs
    --     Nothing -> putStrLn "Provide a category"
    _ -> putStrLn "Not a valid report type"
  where
    subcategories cat = getSubcategoryBreakdown (categoryFromString cat)