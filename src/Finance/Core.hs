{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Eta reduce" #-}
module Finance.Core
  (filterTransactions
  , spendingByCategory 
  , spendingByPurchaseCategory
  , getSubcategoryBreakdown
  ) where

import Data.Decimal (Decimal)
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time
import Data.Time.Calendar.Month
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

dateWithin :: Day -> Day -> (Transaction -> Bool)
dateWithin d1 d2 = \t -> txDate t <= d2 && txDate t >= d1

amountEquals :: Decimal -> (Transaction -> Bool)
amountEquals amt = \t -> txAmount t == amt

titleInfix :: T.Text -> (Transaction -> Bool)
titleInfix title = \t -> title `T.isInfixOf` txTitle t

-- Combine predicates to a single predicate
combinePredicateFilters :: [Transaction -> Bool] -> (Transaction -> Bool)
combinePredicateFilters predicates = \transaction -> Prelude.all (\p -> p transaction) predicates

-- Filter a list of transactions
filterTransactions :: [Transaction] -> (Transaction -> Bool) -> [Transaction]
filterTransactions txs filters = filter filters txs

---------------
-- Summarizing
---------------

-- Aggregate transaction amounts by category
spendingByCategory :: [Transaction] -> AggregatedSpending
spendingByCategory = foldl f Map.empty
  where
    f m t = Map.insertWith (+) (getBudgetCategory $ txCategory t) (txAmount t) m

spendingByPurchaseCategory :: [Transaction] -> AggregatedSpending
spendingByPurchaseCategory = foldl f Map.empty
  where
    f m t = Map.insertWith (+) (txCategory t) (txAmount t) m

getSubcategoryBreakdown :: Category -> AggregatedSpending -> AggregatedSpending
getSubcategoryBreakdown targetC purchaseSpendingMap =
  Map.filterWithKey (\c _ -> getBudgetCategory c == targetC) purchaseSpendingMap

-- Compare transaction spending by category and a budget
budgetVersusSpending
  :: AggregatedSpending
  -> BudgetedAmounts
  -> Map.Map Category BudgetComparison
budgetVersusSpending actualSpendingMap budgetedSpendingMap =
  Map.fromList $
    map checkDiscrepancy categories
  where
    -- Extract categories by concatenating the keys of each map and removing duplicates (`nub`)
    categories = nub $ Map.keys actualSpendingMap ++ Map.keys budgetedSpendingMap
    -- Return a map from the category to a BudgetComparison containing
    -- the category, actual spending, budgeted, and the difference
    checkDiscrepancy c = (c, BudgetComparison c a b diff)
      where
        a = Map.findWithDefault 0 c actualSpendingMap
        b = Map.findWithDefault 0 c budgetedSpendingMap
        diff = b - a

totalSpending :: [Transaction] -> Decimal
totalSpending txs = foldl' (\acc tx -> acc + txAmount tx) (0 :: Decimal) txs

---------------
-- Refining categories
---------------

categoryMap :: Map.Map Category Category
categoryMap =
  Map.fromList
    [ (categoryFromString "CasualMeal", categoryFromString "Food")
    , (categoryFromString "CoolMeal", categoryFromString "Food")
    , (categoryFromString "Groceries", categoryFromString "Food")
    , (categoryFromString "Snacks", categoryFromString "Food")
    ]

-- Find a specific category's parent category
getBudgetCategory :: Category -> Category
getBudgetCategory cat = Map.findWithDefault cat cat categoryMap

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
  , Transaction
      (T.pack "Uber")
      testDate2
      (26.10 :: Decimal)
      (categoryFromString "Transport")
  ]

testBudget =
  Budget
    (MkMonth 24305)
    ( Map.fromList
        [ (categoryFromString "CasualMeal", 100.0 :: Decimal)
        , (categoryFromString "Groceries", 50.0 :: Decimal)
        ]
    )