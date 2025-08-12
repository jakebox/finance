{-# LANGUAGE TemplateHaskell #-}

module Finance.TUI.Types where

import qualified Data.Text as T
import Lens.Micro.TH (makeLenses)
import Brick.Widgets.List (List)
import Finance.Types (Transaction, BudgetMap, Filter, BudgetComparison, Category)
import qualified Data.Map.Strict as Map
import Data.Time.Calendar.Month (Month)

data Name = TransactionList | Budget
  deriving (Eq, Ord, Show)

data AppState = AppState
  { _transactions :: [Transaction]
  , _transactionList :: List Name Transaction
  , _budget :: BudgetMap
  , _budgetComparison :: Maybe (Map.Map Category BudgetComparison)
  , _filters :: [Filter]
  , _statusMessage :: Maybe T.Text
  , _currentMonth :: Month
  } deriving (Show)

makeLenses ''AppState