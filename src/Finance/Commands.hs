{-# LANGUAGE NamedFieldPuns #-}

module Finance.Commands (runReport, reportCommandParser, runBudget, budgetCommandParser, Command (..)) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Finance.Core
import Finance.Input
import Finance.PrettyPrint
import Finance.Types

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

runBudget :: BudgetCommandOptions -> IO ()
runBudget BudgetCommandOptions {bAction, bMonth} = do
  txs <- readTransactionFile transactionsFile
  case bAction of
    "check" -> do
      let comparison = budgetVersusSpending (spendingByCategory txs) (bd Finance.Core.testBudget)
      T.putStrLn $
        T.intercalate "\n\n" $
          map (\(_, b) -> ppBudgetComparison b) (Map.toList comparison)
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
