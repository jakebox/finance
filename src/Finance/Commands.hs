{-# LANGUAGE NamedFieldPuns #-}

module Finance.Commands (runReport, reportCommandParser, runBudget, budgetCommandParser, Command (..)) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Maybe
import Data.Time.Calendar.Month
import Data.Time
import Finance.Core
import Finance.Input
import Finance.PrettyPrint
import Finance.Types

import Data.Time (getCurrentTime, toGregorian)
import Options.Applicative

transactionsFile :: FilePath = "transactions.csv"

data Command
  = ReportCommand ReportCommandOptions
  | BudgetCommand BudgetCommandOptions

data BudgetCommandOptions = BudgetCommandOptions
  { bAction :: String
  , bMonth :: String
  }

data ReportCommandOptions = ReportCommandOptions
  { rType :: String
  , --  , rCategory :: Maybe String
    rFilters :: Maybe String
  }

reportCommandParser :: Parser ReportCommandOptions
reportCommandParser =
  ReportCommandOptions
    <$> argument str (metavar "TYPE")
    --    <*> optional (argument str (metavar "CATEGORY"))
    <*> optional (argument str (metavar "FILTERS"))

budgetCommandParser :: Parser BudgetCommandOptions
budgetCommandParser =
  BudgetCommandOptions
    <$> argument str (metavar "ACTION")
    <*> argument str (metavar "MONTH")

today :: IO Day
today = getCurrentTimeZone >>= \t -> localDay . utcToLocalTime t <$> getCurrentTime

runBudget :: BudgetCommandOptions -> IO ()
runBudget BudgetCommandOptions {bAction, bMonth} = do
  txs <- readTransactionFile transactionsFile
  today <- today
  let month = stringToYearMonth bMonth
      budget = fromJust $ Map.lookup (MkMonth 24306) Finance.Core.testBudget
  case bAction of
    "check" -> do
      let filtered_txs = filterTransactions txs (matchesMonthYear month)
          comparison = budgetVersusSpending (spendingByCategory filtered_txs) budget
          eff = categoryMonthBudgetEfficency today (MkMonth 24306) comparison
      printBudgetTable comparison
      printEfficiency "Overall efficency: " eff
    _ -> putStrLn "Not a valid budget command"

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
