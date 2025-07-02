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
    <*> optional (argument str (metavar "CATEGORY"))

runReport :: ReportCommandOptions -> IO ()
runReport ReportCommandOptions {rType, rCategory} = do
  let fp = "transactions.csv"
  txs <- readTransactionFile fp
  case rType of
    "summary" -> print $ spendingByCategory txs
    "category" ->
      case rCategory of
        Just category -> print $ subcategories category . spendingByPurchaseCategory $ txs
        Nothing -> putStrLn "Provide a category"
    _ -> putStrLn "Not a valid category type"
  where
    subcategories cat = getSubcategoryBreakdown (categoryFromString cat)