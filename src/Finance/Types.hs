{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Finance.Types
  ( Category (..)
  , Transaction (..)
  , Budget (..)
  , BudgetComparison (..)
  , categoryFromString
  , AggregatedSpending
  , BudgetedAmounts
  , Filter
  , TransactionFilterP
  , getCategoryText
  ) where

import Data.Decimal (Decimal)
import Data.Map.Strict (Map)
import qualified Data.Text as T
import Data.Time
import Data.Time.Calendar.Month

newtype Category = Category T.Text
  deriving (Eq, Ord, Show)

getCategoryText :: Category -> T.Text
getCategoryText (Category txt) = txt

categoryFromString :: String -> Category
categoryFromString s = Category $ T.pack s

data Transaction = Transaction
  { txTitle :: T.Text
  , txDate :: Day
  , txAmount :: Decimal
  , txCategory :: Category
  , txNote :: T.Text
  }
  deriving (Eq, Show)

data Budget = Budget
  { bdDate :: Month
  , bd :: BudgetedAmounts
  }
  deriving (Eq, Show)

data BudgetComparison = BudgetComparison
  { category :: Category
  , actual :: Decimal
  , budgeted :: Decimal
  , difference :: Decimal
  }
  deriving (Eq, Ord, Show)

type AggregatedSpending = Map Category Decimal
type BudgetedAmounts = Map Category Decimal

data Filter
  = FilterDate Day
  | MonthFilter MonthOfYear
  | DateRangeFilter Day Day
  | AmountFilter Decimal
  | TitleFilter T.Text
  deriving (Show, Eq)

type TransactionFilterP = Transaction -> Bool