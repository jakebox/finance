module Finance.PrettyPrint (ppTransaction, ppAggregatedSpending, ppBudgetComparison) where

import Data.Decimal (Decimal)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T

import Finance.Types

{-

\$24.55 - Trader Joe's | 2025-05-26 (Groceries)

-}
ppTransaction :: Transaction -> T.Text
ppTransaction tx =
  (T.pack $ show (tx.txAmount))
    <> T.pack "-"
    <> txTitle tx
    <> T.pack " | "
    <> (T.pack $ show (tx.txDate))
    <> (T.pack $ show (tx.txCategory))

ppAggregatedSpending :: AggregatedSpending -> T.Text
ppAggregatedSpending ag
  | M.null ag = T.empty
  | otherwise = T.intercalate "\n" (header : lines)
  where
    lines = M.foldrWithKey acc [] ag
    categoryPad = 16
    header = (T.justifyLeft categoryPad ' ' "Category") <> (T.justifyRight 7 ' ' "Amount")
    acc :: Category -> Decimal -> [T.Text] -> [T.Text]
    acc cat amt acc_lines =
      let categoryText = getCategoryText cat
          amountText = T.pack $ show amt
          newLine =
            T.justifyLeft categoryPad ' ' (categoryText <> ": ")
              <> T.justifyRight 7 ' ' amountText
       in newLine : acc_lines

{-
Discretionary:  $55.43
Food:          $519.56
Necessities:    $34.10
Social:         $37.23
Transport:     $273.65
-}

ppBudgetComparison :: BudgetComparison -> T.Text
ppBudgetComparison bc =
  T.intercalate
    "\n"
    [ T.justifyLeft 14 ' ' c
    , T.justifyLeft 14 ' ' "\tBudgeted: " <> b
    , T.justifyLeft 14 ' ' "\tSpent: " <> a
    , T.justifyLeft 14 ' ' "\tRemainder: " <> r
    ]
  where
    c = getCategoryText bc.category
    b = T.justifyRight 6 ' ' $ T.pack $ show bc.budgeted
    a = T.justifyRight 6 ' ' $ T.pack $ show bc.actual
    r = T.justifyRight 6 ' ' $ T.pack $ show bc.difference

{-

 Budgeted:  300.00 (b.budgeted)
    Spent: - 30.00 (b.actual)
Remainder:  270.00 (b.difference)

-}
