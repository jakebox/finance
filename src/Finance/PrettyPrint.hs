module Finance.PrettyPrint (ppTransaction, ppAggregatedSpending, ppBudgetComparison, printBudgetTable, printEfficiency) where

import Data.Decimal (Decimal)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Printf

import Finance.Types
import Finance.Core

{-

$24.55 - Trader Joe's | 2025-05-26 (Groceries)

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
    , T.justifyLeft 14 ' ' "  Budgeted: " <> b
    , T.justifyLeft 14 ' ' "  Spent: " <> a
    , T.justifyLeft 14 ' ' "  Remainder: " <> r
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

  -- ANSI-colored bar
spentBar :: Float -> Int -> T.Text
spentBar pct width =
  let filled = round (pct * fromIntegral width / 100)
      empty  = width - filled
      bar    = replicate filled '█' ++ replicate empty '░'
      color
        | pct > 75  = "\ESC[31m" -- red
        | pct > 25  = "\ESC[33m" -- yellow
        | otherwise = "\ESC[32m" -- green
  in T.pack $ color ++ bar ++ "\ESC[0m"

-- One table row with colored bar
ppRow :: BudgetComparison -> T.Text
ppRow bc =
  let b = realToFrac bc.budgeted   :: Float
      a = realToFrac bc.actual     :: Float
      r = realToFrac bc.difference :: Float
      pct = if b == 0 then 0 else (a / b) * 100
  in T.pack $
       printf "%-14s | %8.2f | %8.2f | %9.2f | %s"
              (T.unpack $ getCategoryText bc.category)
              b a r
              (T.unpack $ spentBar pct 10)

ppTable :: [BudgetComparison] -> T.Text
ppTable rows =
  let bold = "\ESC[1m"
      reset = "\ESC[0m"
  in T.unlines $
       [ T.pack $ bold ++ "Category       | Budgeted |  Spent   | Remainder | Spent %" ++ reset
       , "---------------+----------+----------+-----------+-----------"
       ] ++ map ppRow rows

-- Entry point
printBudgetTable :: M.Map Category BudgetComparison -> IO ()
printBudgetTable comparison =
  let rows = map snd $ M.toList comparison
  in T.putStrLn (ppTable rows)


printEfficiency :: String -> Float -> IO ()
printEfficiency leader eff = T.putStrLn $
  T.pack leader <> ": " <>
  T.pack (printf "%+0.1f%%  " (eff * 100)) <>
  italic (T.pack $ "(" <> explanation <> ")")
  where
    explanation = efficiencyToExplanation eff
    italic txt = "\ESC[3m" <> txt <> "\ESC[0m"
