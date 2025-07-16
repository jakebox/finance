{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Eta reduce" #-}
module Finance.Core
  ( filterTransactions
  , spendingByCategory
  , spendingByPurchaseCategory
  , getSubcategoryBreakdown
  , combinePredicateFilters
  , amountEquals
  , matchesMonth
  , matchesMonthYear
  , titleInfix
  , identityFilter
  , budgetVersusSpending
  , testBudget
  , categoryMonthBudgetEfficency
  , categoryBudgetEfficency
  , efficiencyToExplanation
  ) where

import Data.Decimal (Decimal, (*.))
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Data.Time.Calendar.Month
import Finance.Types
import Finance.Utils

----------------
-- Filtering
----------------

-- Filter predicates

identityFilter :: TransactionFilterP
identityFilter = \t -> True

matchesDate :: Day -> TransactionFilterP
matchesDate targetDate = \t -> txDate t == targetDate

matchesMonth :: MonthOfYear -> TransactionFilterP
matchesMonth targetMonth = \t -> case toGregorian $ txDate t of
  (_, m, _) -> m == targetMonth

dateWithin :: Day -> Day -> TransactionFilterP
dateWithin d1 d2 = \t -> txDate t <= d2 && txDate t >= d1

amountEquals :: Decimal -> TransactionFilterP
amountEquals amt = \t -> txAmount t == amt

matchesMonthYear :: Month -> TransactionFilterP
matchesMonthYear my = \t -> case toGregorian $ txDate t of
  (y, m, _) -> YearMonth y m == my

titleInfix :: T.Text -> TransactionFilterP
titleInfix title = \t -> title `T.isInfixOf` txTitle t

-- Combine predicates to a single predicate
combinePredicateFilters :: [TransactionFilterP] -> TransactionFilterP
combinePredicateFilters predicates = \transaction -> Prelude.all (\p -> p transaction) predicates

-- Filter a list of transactions
filterTransactions :: [Transaction] -> (TransactionFilterP) -> [Transaction]
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

{- | Calculate the efficency metric for a single BugetComparison.
  Based on how far through the month we are and the spending.
-}
categoryBudgetEfficency :: DayOfMonth -> BudgetComparison -> Float
categoryBudgetEfficency today bc = 1 - actual / expected
  where
    actual = realToFrac bc.actual
    expected = realToFrac bc.budgeted * monthPct
    monthPct = realToFrac today / 30

categoryMonthBudgetEfficency
  :: Day -> Month -> Map.Map Category BudgetComparison -> Float
categoryMonthBudgetEfficency today budgetMonth mbc
  | isPastBudget = 1 - (realToFrac sumSpend / realToFrac sumBudget)
  | otherwise = 1 - (realToFrac sumSpend / expected)
  where
    isPastBudget = budgetMonth < thisMonth
    (y, m, d) = toGregorian today
    thisMonth = fromMaybe (MkMonth 100000) $ fromYearMonthValid y m
    expected = realToFrac sumBudget * (monthPct :: Float)
    monthPct = fromIntegral d / 30.0

    (sumSpend, sumBudget) = foldl' acc (0, 0) (getBudgetComparisonsFromMap mbc)
    acc (ss, sb) (bc :: BudgetComparison) = (ss + bc.actual, sb + bc.budgeted)

efficiencyToExplanation :: Float -> String
efficiencyToExplanation eff
  | eff < -0.1 = "Overspending"
  | eff > -0.1 && eff < 0.1 = "On Track"
  | eff > 0.1 = "Underspending"

-- Budget helper
getBudgetComparisonsFromMap
  :: Map.Map Category BudgetComparison -> [BudgetComparison]
getBudgetComparisonsFromMap bcs = map snd $ Map.toList bcs

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

testBudget :: BudgetMap = Map.fromList[
    (MkMonth 24306, -- June
    ( Map.fromList
        [ (categoryFromString "Food", 488.0 :: Decimal)
        , (categoryFromString "Transport", 275.0 :: Decimal)
        , (categoryFromString "Discretionary", 50.0 :: Decimal)
        , (categoryFromString "Necessities", 30.0 :: Decimal)
        , (categoryFromString "Social", 100.0 :: Decimal)
        ]
    ))]
