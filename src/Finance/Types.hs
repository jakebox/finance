{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Finance.Types
  ( Category (..)
  , Transaction (..)
  , Budget (..)
  , BudgetComparison (..)
  , categoryFromString
  , AggregatedSpending
  , BudgetedAmounts
  ) where

import Data.Decimal (Decimal)
import Data.Map.Strict (Map)
import qualified Data.Text as T (Text, pack)
import Data.Time (Day)
import Data.Time.Calendar.Month (Month)

newtype Category = Category T.Text
  deriving (Eq, Show, Ord)

categoryFromString :: String -> Category
categoryFromString s = Category $ T.pack s

data Transaction = Transaction
  { txTitle :: T.Text
  , txDate :: Day
  , txAmount :: Decimal
  , txCategory :: Category
  }
  deriving (Eq, Show)

data Budget = Budget
  { bdDate :: Month
  , bd :: Map Category Decimal
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
type BudgetedAmounts    = Map Category Decimal