{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Eta reduce" #-}
module Finance.Core
  (
  ) where

import Data.Decimal (Decimal)
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time
import Finance.Types
import Finance.Utils

----------------
-- Filtering
----------------

-- Filter predicates

matchesDate :: Day -> (Transaction -> Bool)
matchesDate targetDate = \t -> txDate t == targetDate

matchesMonth :: MonthOfYear -> (Transaction -> Bool)
matchesMonth targetMonth = \t -> case toGregorian $ txDate t of
  (_, m, _) -> m == targetMonth

amountEquals :: Decimal -> (Transaction -> Bool)
amountEquals amt = \t -> txAmount t == amt

titleInfix :: T.Text -> (Transaction -> Bool)
titleInfix title = \t -> title `T.isInfixOf` txTitle t

-- Combine predicates to a single predicate
combinePredicateFilters :: [Transaction -> Bool] -> (Transaction -> Bool)
combinePredicateFilters predicates = \transaction -> Prelude.all (\p -> p transaction) predicates

-- Filter a list of transactions
filterTransactions :: [Transaction] -> (Transaction -> Bool) -> [Transaction]
filterTransactions transactions filters = filter filters transactions

---------------
-- Summarizing
---------------

-- Aggregate transaction amounts by category
spendingByCategory :: [Transaction] -> Map.Map Category Decimal
spendingByCategory = foldl f Map.empty
  where
    f m t = Map.insertWith (+) (txCategory t) (txAmount t) m

-- Compare transaction spending by category and a budget
budgetVersusSpending
  :: AggregatedSpending
  -> BudgetedAmounts
  -> Map.Map Category BudgetComparison
budgetVersusSpending actualSpendingMap budgetedSpendingMap =
  Map.fromList $
    map checkDiscrepancy categories
  where
    categories = nub $ Map.keys actualSpendingMap ++ Map.keys budgetedSpendingMap
    checkDiscrepancy c = (c, BudgetComparison c a b diff)
      where
        a = Map.findWithDefault 0 c actualSpendingMap
        b = Map.findWithDefault 0 c budgetedSpendingMap
        diff = b - a

-------------- Test data

testDate1 = case dayFromS "2025-04-10" of
  Just d -> d
  Nothing -> error "bad"

testDate2 = case dayFromS "2025-06-10" of
  Just d -> d
  Nothing -> error "bad"

transactions =
  [ Transaction
      (T.pack "Trader Joes")
      testDate1
      (30 :: Decimal)
      (categoryFromString "Groceries")
  , Transaction
      (T.pack "Tacos")
      testDate2
      (13.30 :: Decimal)
      (categoryFromString "CasualMeal")
  ]