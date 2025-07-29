module Finance.Commands
  ( runReport
  , reportCommandParser
  , runBudget
  , budgetCommandParser
  , runTransaction
  , transactionCommandParser
  , Command (..)
  ) where

import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Data.List
import qualified Data.Map.Strict as Map
import Data.Ord
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time
import Finance.Core
import Finance.Input
import Finance.ParseBudgetYaml
import Finance.PrettyPrint
import Finance.Types
import Finance.Utils
import Options.Applicative
import System.IO

transactionsFile :: FilePath = "transactions.csv"
budgetsFile :: FilePath = "budget.yaml"

data Command
  = ReportCommand ReportCommandOptions
  | BudgetCommand BudgetCommandOptions
  | TransactionCommand TransactionCommandOptions

data BudgetCommandOptions = BudgetCommandOptions
  { bAction :: String
  , bMonth :: String
  }

data ReportCommandOptions = ReportCommandOptions
  { rType :: String
  , --  , rCategory :: Maybe String
    rFilters :: Maybe String
  }

newtype TransactionCommandOptions = TransactionCommandOptions
  { tType :: String
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

transactionCommandParser :: Parser TransactionCommandOptions
transactionCommandParser =
  TransactionCommandOptions
    <$> argument str (metavar "COMMAND")

runBudget :: BudgetCommandOptions -> IO ()
runBudget BudgetCommandOptions {bAction, bMonth} = do
  txs <- readTransactionFile transactionsFile
  budgets <-
    parseBudgetYaml budgetsFile >>= \case
      Left err -> error $ show err
      Right bud -> return bud
  today <- today
  monthMaybe <- parseMonthFallbackToThisYear bMonth
  let month = case monthMaybe of
                Left err -> error $ show err
                Right m -> m
  let  maybeBudget = Map.lookup month budgets
  case maybeBudget of
    Just budget ->
      case bAction of
        "check" -> do
          let filtered_txs = filterTransactions txs (matchesMonthYear month)
              comparison = budgetVersusSpending (spendingByCategory filtered_txs) budget
              eff = categoryMonthBudgetEfficency today month comparison
          printBudgetTable comparison
          printEfficiency "Overall efficency: " eff
        _ -> putStrLn "Not a valid budget command"
    Nothing -> putStrLn "Could not find a budget for requested month."

runTransaction :: TransactionCommandOptions -> IO ()
runTransaction TransactionCommandOptions {tType} = do
  case tType of
    "add" -> do
      hSetBuffering stdout NoBuffering
      result <- runExceptT runAddTx
      case result of
        Left err -> T.putStrLn "Error on input"
        Right tx -> do
          addTxToTransactionFile transactionsFile tx
          T.putStrLn $ "Added: " <> ppTransaction tx
    _ -> putStrLn "Not a valid transaction command."

runReport :: ReportCommandOptions -> IO ()
runReport ReportCommandOptions {rType, rFilters} = do
  txs <- readTransactionFile transactionsFile
  let filtered_txs = filterTransactions txs (stringToFilters rFilters)
  case rType of
    "summary" -> T.putStrLn . ppAggregatedSpending $ spendingByCategory filtered_txs
    "list" -> mapM_ (T.putStrLn . ppTransaction) (sortBy (comparing $ Down . txAmount) filtered_txs)
    -- "category" ->
    --   case rCategory of
    --     Just category -> print $ subcategories category . spendingByPurchaseCategory $ txs
    --     Nothing -> putStrLn "Provide a category"
    _ -> putStrLn "Not a valid report type"
  where
    subcategories cat = getSubcategoryBreakdown (categoryFromString cat)
